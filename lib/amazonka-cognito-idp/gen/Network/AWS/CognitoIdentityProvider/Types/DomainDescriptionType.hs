-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
  ( DomainDescriptionType (..),

    -- * Smart constructor
    mkDomainDescriptionType,

    -- * Lenses
    ddtStatus,
    ddtCloudFrontDistribution,
    ddtUserPoolId,
    ddtDomain,
    ddtAWSAccountId,
    ddtCustomDomainConfig,
    ddtVersion,
    ddtS3Bucket,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for information about a domain.
--
-- /See:/ 'mkDomainDescriptionType' smart constructor.
data DomainDescriptionType = DomainDescriptionType'
  { status ::
      Lude.Maybe DomainStatusType,
    cloudFrontDistribution :: Lude.Maybe Lude.Text,
    userPoolId :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    awsAccountId :: Lude.Maybe Lude.Text,
    customDomainConfig ::
      Lude.Maybe CustomDomainConfigType,
    version :: Lude.Maybe Lude.Text,
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainDescriptionType' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The AWS account ID for the user pool owner.
-- * 'cloudFrontDistribution' - The ARN of the CloudFront distribution.
-- * 'customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
-- * 'domain' - The domain string.
-- * 's3Bucket' - The S3 bucket where the static files for this domain are stored.
-- * 'status' - The domain status.
-- * 'userPoolId' - The user pool ID.
-- * 'version' - The app version.
mkDomainDescriptionType ::
  DomainDescriptionType
mkDomainDescriptionType =
  DomainDescriptionType'
    { status = Lude.Nothing,
      cloudFrontDistribution = Lude.Nothing,
      userPoolId = Lude.Nothing,
      domain = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      customDomainConfig = Lude.Nothing,
      version = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | The domain status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtStatus :: Lens.Lens' DomainDescriptionType (Lude.Maybe DomainStatusType)
ddtStatus = Lens.lens (status :: DomainDescriptionType -> Lude.Maybe DomainStatusType) (\s a -> s {status = a} :: DomainDescriptionType)
{-# DEPRECATED ddtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the CloudFront distribution.
--
-- /Note:/ Consider using 'cloudFrontDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtCloudFrontDistribution :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtCloudFrontDistribution = Lens.lens (cloudFrontDistribution :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {cloudFrontDistribution = a} :: DomainDescriptionType)
{-# DEPRECATED ddtCloudFrontDistribution "Use generic-lens or generic-optics with 'cloudFrontDistribution' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtUserPoolId :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtUserPoolId = Lens.lens (userPoolId :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: DomainDescriptionType)
{-# DEPRECATED ddtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtDomain :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtDomain = Lens.lens (domain :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: DomainDescriptionType)
{-# DEPRECATED ddtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The AWS account ID for the user pool owner.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtAWSAccountId :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtAWSAccountId = Lens.lens (awsAccountId :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: DomainDescriptionType)
{-# DEPRECATED ddtAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- /Note:/ Consider using 'customDomainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtCustomDomainConfig :: Lens.Lens' DomainDescriptionType (Lude.Maybe CustomDomainConfigType)
ddtCustomDomainConfig = Lens.lens (customDomainConfig :: DomainDescriptionType -> Lude.Maybe CustomDomainConfigType) (\s a -> s {customDomainConfig = a} :: DomainDescriptionType)
{-# DEPRECATED ddtCustomDomainConfig "Use generic-lens or generic-optics with 'customDomainConfig' instead." #-}

-- | The app version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtVersion :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtVersion = Lens.lens (version :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DomainDescriptionType)
{-# DEPRECATED ddtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The S3 bucket where the static files for this domain are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtS3Bucket :: Lens.Lens' DomainDescriptionType (Lude.Maybe Lude.Text)
ddtS3Bucket = Lens.lens (s3Bucket :: DomainDescriptionType -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: DomainDescriptionType)
{-# DEPRECATED ddtS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromJSON DomainDescriptionType where
  parseJSON =
    Lude.withObject
      "DomainDescriptionType"
      ( \x ->
          DomainDescriptionType'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "CloudFrontDistribution")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "Domain")
            Lude.<*> (x Lude..:? "AWSAccountId")
            Lude.<*> (x Lude..:? "CustomDomainConfig")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "S3Bucket")
      )
