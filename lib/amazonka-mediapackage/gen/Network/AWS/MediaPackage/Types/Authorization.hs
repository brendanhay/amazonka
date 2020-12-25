{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Authorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Authorization
  ( Authorization (..),

    -- * Smart constructor
    mkAuthorization,

    -- * Lenses
    aSecretsRoleArn,
    aCdnIdentifierSecret,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | CDN Authorization credentials
--
-- /See:/ 'mkAuthorization' smart constructor.
data Authorization = Authorization'
  { -- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage to communicate with AWS Secrets Manager.
    secretsRoleArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) for the secret in Secrets Manager that your Content Distribution Network (CDN) uses for authorization to access your endpoint.
    cdnIdentifierSecret :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Authorization' value with any optional fields omitted.
mkAuthorization ::
  -- | 'secretsRoleArn'
  Core.Text ->
  -- | 'cdnIdentifierSecret'
  Core.Text ->
  Authorization
mkAuthorization secretsRoleArn cdnIdentifierSecret =
  Authorization' {secretsRoleArn, cdnIdentifierSecret}

-- | The Amazon Resource Name (ARN) for the IAM role that allows MediaPackage to communicate with AWS Secrets Manager.
--
-- /Note:/ Consider using 'secretsRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSecretsRoleArn :: Lens.Lens' Authorization Core.Text
aSecretsRoleArn = Lens.field @"secretsRoleArn"
{-# DEPRECATED aSecretsRoleArn "Use generic-lens or generic-optics with 'secretsRoleArn' instead." #-}

-- | The Amazon Resource Name (ARN) for the secret in Secrets Manager that your Content Distribution Network (CDN) uses for authorization to access your endpoint.
--
-- /Note:/ Consider using 'cdnIdentifierSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCdnIdentifierSecret :: Lens.Lens' Authorization Core.Text
aCdnIdentifierSecret = Lens.field @"cdnIdentifierSecret"
{-# DEPRECATED aCdnIdentifierSecret "Use generic-lens or generic-optics with 'cdnIdentifierSecret' instead." #-}

instance Core.FromJSON Authorization where
  toJSON Authorization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("secretsRoleArn" Core..= secretsRoleArn),
            Core.Just ("cdnIdentifierSecret" Core..= cdnIdentifierSecret)
          ]
      )

instance Core.FromJSON Authorization where
  parseJSON =
    Core.withObject "Authorization" Core.$
      \x ->
        Authorization'
          Core.<$> (x Core..: "secretsRoleArn")
          Core.<*> (x Core..: "cdnIdentifierSecret")
