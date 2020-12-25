{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthorizationConfig
  ( AuthorizationConfig (..),

    -- * Smart constructor
    mkAuthorizationConfig,

    -- * Lenses
    acAuthorizationType,
    acAwsIamConfig,
  )
where

import qualified Network.AWS.AppSync.Types.AuthorizationType as Types
import qualified Network.AWS.AppSync.Types.AwsIamConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorization config in case the HTTP endpoint requires authorization.
--
-- /See:/ 'mkAuthorizationConfig' smart constructor.
data AuthorizationConfig = AuthorizationConfig'
  { -- | The authorization type required by the HTTP endpoint.
    --
    --
    --     * __AWS_IAM__ : The authorization type is Sigv4.
    authorizationType :: Types.AuthorizationType,
    -- | The AWS IAM settings.
    awsIamConfig :: Core.Maybe Types.AwsIamConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizationConfig' value with any optional fields omitted.
mkAuthorizationConfig ::
  -- | 'authorizationType'
  Types.AuthorizationType ->
  AuthorizationConfig
mkAuthorizationConfig authorizationType =
  AuthorizationConfig'
    { authorizationType,
      awsIamConfig = Core.Nothing
    }

-- | The authorization type required by the HTTP endpoint.
--
--
--     * __AWS_IAM__ : The authorization type is Sigv4.
--
--
--
-- /Note:/ Consider using 'authorizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAuthorizationType :: Lens.Lens' AuthorizationConfig Types.AuthorizationType
acAuthorizationType = Lens.field @"authorizationType"
{-# DEPRECATED acAuthorizationType "Use generic-lens or generic-optics with 'authorizationType' instead." #-}

-- | The AWS IAM settings.
--
-- /Note:/ Consider using 'awsIamConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAwsIamConfig :: Lens.Lens' AuthorizationConfig (Core.Maybe Types.AwsIamConfig)
acAwsIamConfig = Lens.field @"awsIamConfig"
{-# DEPRECATED acAwsIamConfig "Use generic-lens or generic-optics with 'awsIamConfig' instead." #-}

instance Core.FromJSON AuthorizationConfig where
  toJSON AuthorizationConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("authorizationType" Core..= authorizationType),
            ("awsIamConfig" Core..=) Core.<$> awsIamConfig
          ]
      )

instance Core.FromJSON AuthorizationConfig where
  parseJSON =
    Core.withObject "AuthorizationConfig" Core.$
      \x ->
        AuthorizationConfig'
          Core.<$> (x Core..: "authorizationType")
          Core.<*> (x Core..:? "awsIamConfig")
