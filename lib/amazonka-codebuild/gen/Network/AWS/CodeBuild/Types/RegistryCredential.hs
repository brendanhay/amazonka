{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RegistryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RegistryCredential
  ( RegistryCredential (..),

    -- * Smart constructor
    mkRegistryCredential,

    -- * Lenses
    rcCredential,
    rcCredentialProvider,
  )
where

import qualified Network.AWS.CodeBuild.Types.Credential as Types
import qualified Network.AWS.CodeBuild.Types.CredentialProviderType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about credentials that provide access to a private Docker registry. When this is set:
--
--
--     * @imagePullCredentialsType@ must be set to @SERVICE_ROLE@ .
--
--
--     * images cannot be curated or an Amazon ECR image.
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-private-registry.html Private Registry with AWS Secrets Manager Sample for AWS CodeBuild> .
--
-- /See:/ 'mkRegistryCredential' smart constructor.
data RegistryCredential = RegistryCredential'
  { -- | The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
    credential :: Types.Credential,
    -- | The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
    credentialProvider :: Types.CredentialProviderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegistryCredential' value with any optional fields omitted.
mkRegistryCredential ::
  -- | 'credential'
  Types.Credential ->
  -- | 'credentialProvider'
  Types.CredentialProviderType ->
  RegistryCredential
mkRegistryCredential credential credentialProvider =
  RegistryCredential' {credential, credentialProvider}

-- | The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredential :: Lens.Lens' RegistryCredential Types.Credential
rcCredential = Lens.field @"credential"
{-# DEPRECATED rcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
--
-- /Note:/ Consider using 'credentialProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredentialProvider :: Lens.Lens' RegistryCredential Types.CredentialProviderType
rcCredentialProvider = Lens.field @"credentialProvider"
{-# DEPRECATED rcCredentialProvider "Use generic-lens or generic-optics with 'credentialProvider' instead." #-}

instance Core.FromJSON RegistryCredential where
  toJSON RegistryCredential {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("credential" Core..= credential),
            Core.Just ("credentialProvider" Core..= credentialProvider)
          ]
      )

instance Core.FromJSON RegistryCredential where
  parseJSON =
    Core.withObject "RegistryCredential" Core.$
      \x ->
        RegistryCredential'
          Core.<$> (x Core..: "credential") Core.<*> (x Core..: "credentialProvider")
