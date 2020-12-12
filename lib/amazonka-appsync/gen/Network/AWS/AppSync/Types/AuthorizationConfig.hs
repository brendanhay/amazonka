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
    acAwsIAMConfig,
    acAuthorizationType,
  )
where

import Network.AWS.AppSync.Types.AWSIAMConfig
import Network.AWS.AppSync.Types.AuthorizationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorization config in case the HTTP endpoint requires authorization.
--
-- /See:/ 'mkAuthorizationConfig' smart constructor.
data AuthorizationConfig = AuthorizationConfig'
  { awsIAMConfig ::
      Lude.Maybe AWSIAMConfig,
    authorizationType :: AuthorizationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizationConfig' with the minimum fields required to make a request.
--
-- * 'authorizationType' - The authorization type required by the HTTP endpoint.
--
--
--     * __AWS_IAM__ : The authorization type is Sigv4.
--
--
-- * 'awsIAMConfig' - The AWS IAM settings.
mkAuthorizationConfig ::
  -- | 'authorizationType'
  AuthorizationType ->
  AuthorizationConfig
mkAuthorizationConfig pAuthorizationType_ =
  AuthorizationConfig'
    { awsIAMConfig = Lude.Nothing,
      authorizationType = pAuthorizationType_
    }

-- | The AWS IAM settings.
--
-- /Note:/ Consider using 'awsIAMConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAwsIAMConfig :: Lens.Lens' AuthorizationConfig (Lude.Maybe AWSIAMConfig)
acAwsIAMConfig = Lens.lens (awsIAMConfig :: AuthorizationConfig -> Lude.Maybe AWSIAMConfig) (\s a -> s {awsIAMConfig = a} :: AuthorizationConfig)
{-# DEPRECATED acAwsIAMConfig "Use generic-lens or generic-optics with 'awsIAMConfig' instead." #-}

-- | The authorization type required by the HTTP endpoint.
--
--
--     * __AWS_IAM__ : The authorization type is Sigv4.
--
--
--
-- /Note:/ Consider using 'authorizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAuthorizationType :: Lens.Lens' AuthorizationConfig AuthorizationType
acAuthorizationType = Lens.lens (authorizationType :: AuthorizationConfig -> AuthorizationType) (\s a -> s {authorizationType = a} :: AuthorizationConfig)
{-# DEPRECATED acAuthorizationType "Use generic-lens or generic-optics with 'authorizationType' instead." #-}

instance Lude.FromJSON AuthorizationConfig where
  parseJSON =
    Lude.withObject
      "AuthorizationConfig"
      ( \x ->
          AuthorizationConfig'
            Lude.<$> (x Lude..:? "awsIamConfig")
            Lude.<*> (x Lude..: "authorizationType")
      )

instance Lude.ToJSON AuthorizationConfig where
  toJSON AuthorizationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("awsIamConfig" Lude..=) Lude.<$> awsIAMConfig,
            Lude.Just ("authorizationType" Lude..= authorizationType)
          ]
      )
