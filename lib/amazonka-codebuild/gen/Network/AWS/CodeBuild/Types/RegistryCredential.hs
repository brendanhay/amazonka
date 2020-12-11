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

import Network.AWS.CodeBuild.Types.CredentialProviderType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { credential ::
      Lude.Text,
    credentialProvider :: CredentialProviderType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegistryCredential' with the minimum fields required to make a request.
--
-- * 'credential' - The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
-- * 'credentialProvider' - The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
mkRegistryCredential ::
  -- | 'credential'
  Lude.Text ->
  -- | 'credentialProvider'
  CredentialProviderType ->
  RegistryCredential
mkRegistryCredential pCredential_ pCredentialProvider_ =
  RegistryCredential'
    { credential = pCredential_,
      credentialProvider = pCredentialProvider_
    }

-- | The Amazon Resource Name (ARN) or name of credentials created using AWS Secrets Manager.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredential :: Lens.Lens' RegistryCredential Lude.Text
rcCredential = Lens.lens (credential :: RegistryCredential -> Lude.Text) (\s a -> s {credential = a} :: RegistryCredential)
{-# DEPRECATED rcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | The service that created the credentials to access a private Docker registry. The valid value, SECRETS_MANAGER, is for AWS Secrets Manager.
--
-- /Note:/ Consider using 'credentialProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCredentialProvider :: Lens.Lens' RegistryCredential CredentialProviderType
rcCredentialProvider = Lens.lens (credentialProvider :: RegistryCredential -> CredentialProviderType) (\s a -> s {credentialProvider = a} :: RegistryCredential)
{-# DEPRECATED rcCredentialProvider "Use generic-lens or generic-optics with 'credentialProvider' instead." #-}

instance Lude.FromJSON RegistryCredential where
  parseJSON =
    Lude.withObject
      "RegistryCredential"
      ( \x ->
          RegistryCredential'
            Lude.<$> (x Lude..: "credential") Lude.<*> (x Lude..: "credentialProvider")
      )

instance Lude.ToJSON RegistryCredential where
  toJSON RegistryCredential' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("credential" Lude..= credential),
            Lude.Just ("credentialProvider" Lude..= credentialProvider)
          ]
      )
