{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ddtAWSAccountId,
    ddtCloudFrontDistribution,
    ddtCustomDomainConfig,
    ddtDomain,
    ddtS3Bucket,
    ddtStatus,
    ddtUserPoolId,
    ddtVersion,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.AWSAccountId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CloudFrontDistribution as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Domain as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DomainStatusType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DomainVersionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.S3BucketType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for information about a domain.
--
-- /See:/ 'mkDomainDescriptionType' smart constructor.
data DomainDescriptionType = DomainDescriptionType'
  { -- | The AWS account ID for the user pool owner.
    aWSAccountId :: Core.Maybe Types.AWSAccountId,
    -- | The ARN of the CloudFront distribution.
    cloudFrontDistribution :: Core.Maybe Types.CloudFrontDistribution,
    -- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
    customDomainConfig :: Core.Maybe Types.CustomDomainConfigType,
    -- | The domain string.
    domain :: Core.Maybe Types.Domain,
    -- | The S3 bucket where the static files for this domain are stored.
    s3Bucket :: Core.Maybe Types.S3BucketType,
    -- | The domain status.
    status :: Core.Maybe Types.DomainStatusType,
    -- | The user pool ID.
    userPoolId :: Core.Maybe Types.UserPoolId,
    -- | The app version.
    version :: Core.Maybe Types.DomainVersionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainDescriptionType' value with any optional fields omitted.
mkDomainDescriptionType ::
  DomainDescriptionType
mkDomainDescriptionType =
  DomainDescriptionType'
    { aWSAccountId = Core.Nothing,
      cloudFrontDistribution = Core.Nothing,
      customDomainConfig = Core.Nothing,
      domain = Core.Nothing,
      s3Bucket = Core.Nothing,
      status = Core.Nothing,
      userPoolId = Core.Nothing,
      version = Core.Nothing
    }

-- | The AWS account ID for the user pool owner.
--
-- /Note:/ Consider using 'aWSAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtAWSAccountId :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.AWSAccountId)
ddtAWSAccountId = Lens.field @"aWSAccountId"
{-# DEPRECATED ddtAWSAccountId "Use generic-lens or generic-optics with 'aWSAccountId' instead." #-}

-- | The ARN of the CloudFront distribution.
--
-- /Note:/ Consider using 'cloudFrontDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtCloudFrontDistribution :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.CloudFrontDistribution)
ddtCloudFrontDistribution = Lens.field @"cloudFrontDistribution"
{-# DEPRECATED ddtCloudFrontDistribution "Use generic-lens or generic-optics with 'cloudFrontDistribution' instead." #-}

-- | The configuration for a custom domain that hosts the sign-up and sign-in webpages for your application.
--
-- /Note:/ Consider using 'customDomainConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtCustomDomainConfig :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.CustomDomainConfigType)
ddtCustomDomainConfig = Lens.field @"customDomainConfig"
{-# DEPRECATED ddtCustomDomainConfig "Use generic-lens or generic-optics with 'customDomainConfig' instead." #-}

-- | The domain string.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtDomain :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.Domain)
ddtDomain = Lens.field @"domain"
{-# DEPRECATED ddtDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The S3 bucket where the static files for this domain are stored.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtS3Bucket :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.S3BucketType)
ddtS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED ddtS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The domain status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtStatus :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.DomainStatusType)
ddtStatus = Lens.field @"status"
{-# DEPRECATED ddtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtUserPoolId :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.UserPoolId)
ddtUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED ddtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The app version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtVersion :: Lens.Lens' DomainDescriptionType (Core.Maybe Types.DomainVersionType)
ddtVersion = Lens.field @"version"
{-# DEPRECATED ddtVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON DomainDescriptionType where
  parseJSON =
    Core.withObject "DomainDescriptionType" Core.$
      \x ->
        DomainDescriptionType'
          Core.<$> (x Core..:? "AWSAccountId")
          Core.<*> (x Core..:? "CloudFrontDistribution")
          Core.<*> (x Core..:? "CustomDomainConfig")
          Core.<*> (x Core..:? "Domain")
          Core.<*> (x Core..:? "S3Bucket")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "UserPoolId")
          Core.<*> (x Core..:? "Version")
