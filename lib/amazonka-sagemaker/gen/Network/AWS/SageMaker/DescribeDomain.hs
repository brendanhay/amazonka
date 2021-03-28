{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The description of the domain.
module Network.AWS.SageMaker.DescribeDomain
    (
    -- * Creating a request
      DescribeDomain (..)
    , mkDescribeDomain
    -- ** Request lenses
    , ddfDomainId

    -- * Destructuring the response
    , DescribeDomainResponse (..)
    , mkDescribeDomainResponse
    -- ** Response lenses
    , ddrrsAppNetworkAccessType
    , ddrrsAuthMode
    , ddrrsCreationTime
    , ddrrsDefaultUserSettings
    , ddrrsDomainArn
    , ddrrsDomainId
    , ddrrsDomainName
    , ddrrsFailureReason
    , ddrrsHomeEfsFileSystemId
    , ddrrsHomeEfsFileSystemKmsKeyId
    , ddrrsKmsKeyId
    , ddrrsLastModifiedTime
    , ddrrsSingleSignOnManagedApplicationInstanceId
    , ddrrsStatus
    , ddrrsSubnetIds
    , ddrrsUrl
    , ddrrsVpcId
    , ddrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomain' value with any optional fields omitted.
mkDescribeDomain
    :: Types.DomainId -- ^ 'domainId'
    -> DescribeDomain
mkDescribeDomain domainId = DescribeDomain'{domainId}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDomainId :: Lens.Lens' DescribeDomain Types.DomainId
ddfDomainId = Lens.field @"domainId"
{-# INLINEABLE ddfDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

instance Core.ToQuery DescribeDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDomain where
        toHeaders DescribeDomain{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeDomain") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDomain where
        toJSON DescribeDomain{..}
          = Core.object
              (Core.catMaybes [Core.Just ("DomainId" Core..= domainId)])

instance Core.AWSRequest DescribeDomain where
        type Rs DescribeDomain = DescribeDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDomainResponse' Core.<$>
                   (x Core..:? "AppNetworkAccessType") Core.<*> x Core..:? "AuthMode"
                     Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "DefaultUserSettings"
                     Core.<*> x Core..:? "DomainArn"
                     Core.<*> x Core..:? "DomainId"
                     Core.<*> x Core..:? "DomainName"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "HomeEfsFileSystemId"
                     Core.<*> x Core..:? "HomeEfsFileSystemKmsKeyId"
                     Core.<*> x Core..:? "KmsKeyId"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "SingleSignOnManagedApplicationInstanceId"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "SubnetIds"
                     Core.<*> x Core..:? "Url"
                     Core.<*> x Core..:? "VpcId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { appNetworkAccessType :: Core.Maybe Types.AppNetworkAccessType
    -- ^ Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
  , authMode :: Core.Maybe Types.AuthMode
    -- ^ The domain's authentication mode.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation time.
  , defaultUserSettings :: Core.Maybe Types.UserSettings
    -- ^ Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile. 
  , domainArn :: Core.Maybe Types.DomainArn
    -- ^ The domain's Amazon Resource Name (ARN).
  , domainId :: Core.Maybe Types.DomainId
    -- ^ The domain ID.
  , domainName :: Core.Maybe Types.DomainName
    -- ^ The domain name.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The failure reason.
  , homeEfsFileSystemId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the Amazon Elastic File System (EFS) managed by this Domain.
  , homeEfsFileSystemKmsKeyId :: Core.Maybe Types.HomeEfsFileSystemKmsKeyId
    -- ^ This member is deprecated and replaced with @KmsKeyId@ .
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified time.
  , singleSignOnManagedApplicationInstanceId :: Core.Maybe Types.String256
    -- ^ The SSO managed application instance ID.
  , status :: Core.Maybe Types.DomainStatus
    -- ^ The status.
  , subnetIds :: Core.Maybe (Core.NonEmpty Types.SubnetId)
    -- ^ The VPC subnets that Studio uses for communication.
  , url :: Core.Maybe Types.Url
    -- ^ The domain's URL.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDomainResponse' value with any optional fields omitted.
mkDescribeDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDomainResponse
mkDescribeDomainResponse responseStatus
  = DescribeDomainResponse'{appNetworkAccessType = Core.Nothing,
                            authMode = Core.Nothing, creationTime = Core.Nothing,
                            defaultUserSettings = Core.Nothing, domainArn = Core.Nothing,
                            domainId = Core.Nothing, domainName = Core.Nothing,
                            failureReason = Core.Nothing, homeEfsFileSystemId = Core.Nothing,
                            homeEfsFileSystemKmsKeyId = Core.Nothing, kmsKeyId = Core.Nothing,
                            lastModifiedTime = Core.Nothing,
                            singleSignOnManagedApplicationInstanceId = Core.Nothing,
                            status = Core.Nothing, subnetIds = Core.Nothing,
                            url = Core.Nothing, vpcId = Core.Nothing, responseStatus}

-- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
--
-- /Note:/ Consider using 'appNetworkAccessType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsAppNetworkAccessType :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.AppNetworkAccessType)
ddrrsAppNetworkAccessType = Lens.field @"appNetworkAccessType"
{-# INLINEABLE ddrrsAppNetworkAccessType #-}
{-# DEPRECATED appNetworkAccessType "Use generic-lens or generic-optics with 'appNetworkAccessType' instead"  #-}

-- | The domain's authentication mode.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsAuthMode :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.AuthMode)
ddrrsAuthMode = Lens.field @"authMode"
{-# INLINEABLE ddrrsAuthMode #-}
{-# DEPRECATED authMode "Use generic-lens or generic-optics with 'authMode' instead"  #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsCreationTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.NominalDiffTime)
ddrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE ddrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile. 
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDefaultUserSettings :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.UserSettings)
ddrrsDefaultUserSettings = Lens.field @"defaultUserSettings"
{-# INLINEABLE ddrrsDefaultUserSettings #-}
{-# DEPRECATED defaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead"  #-}

-- | The domain's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainArn :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainArn)
ddrrsDomainArn = Lens.field @"domainArn"
{-# INLINEABLE ddrrsDomainArn #-}
{-# DEPRECATED domainArn "Use generic-lens or generic-optics with 'domainArn' instead"  #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainId)
ddrrsDomainId = Lens.field @"domainId"
{-# INLINEABLE ddrrsDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainName :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainName)
ddrrsDomainName = Lens.field @"domainName"
{-# INLINEABLE ddrrsDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsFailureReason :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.FailureReason)
ddrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE ddrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- /Note:/ Consider using 'homeEfsFileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsHomeEfsFileSystemId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.ResourceId)
ddrrsHomeEfsFileSystemId = Lens.field @"homeEfsFileSystemId"
{-# INLINEABLE ddrrsHomeEfsFileSystemId #-}
{-# DEPRECATED homeEfsFileSystemId "Use generic-lens or generic-optics with 'homeEfsFileSystemId' instead"  #-}

-- | This member is deprecated and replaced with @KmsKeyId@ .
--
-- /Note:/ Consider using 'homeEfsFileSystemKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsHomeEfsFileSystemKmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.HomeEfsFileSystemKmsKeyId)
ddrrsHomeEfsFileSystemKmsKeyId = Lens.field @"homeEfsFileSystemKmsKeyId"
{-# INLINEABLE ddrrsHomeEfsFileSystemKmsKeyId #-}
{-# DEPRECATED homeEfsFileSystemKmsKeyId "Use generic-lens or generic-optics with 'homeEfsFileSystemKmsKeyId' instead"  #-}

-- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsKmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.KmsKeyId)
ddrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE ddrrsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsLastModifiedTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.NominalDiffTime)
ddrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE ddrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The SSO managed application instance ID.
--
-- /Note:/ Consider using 'singleSignOnManagedApplicationInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsSingleSignOnManagedApplicationInstanceId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.String256)
ddrrsSingleSignOnManagedApplicationInstanceId = Lens.field @"singleSignOnManagedApplicationInstanceId"
{-# INLINEABLE ddrrsSingleSignOnManagedApplicationInstanceId #-}
{-# DEPRECATED singleSignOnManagedApplicationInstanceId "Use generic-lens or generic-optics with 'singleSignOnManagedApplicationInstanceId' instead"  #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsStatus :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainStatus)
ddrrsStatus = Lens.field @"status"
{-# INLINEABLE ddrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The VPC subnets that Studio uses for communication.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsSubnetIds :: Lens.Lens' DescribeDomainResponse (Core.Maybe (Core.NonEmpty Types.SubnetId))
ddrrsSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ddrrsSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The domain's URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsUrl :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.Url)
ddrrsUrl = Lens.field @"url"
{-# INLINEABLE ddrrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsVpcId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.VpcId)
ddrrsVpcId = Lens.field @"vpcId"
{-# INLINEABLE ddrrsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDomainResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
