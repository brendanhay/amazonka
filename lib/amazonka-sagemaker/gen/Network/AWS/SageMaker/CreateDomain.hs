{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Domain@ used by Amazon SageMaker Studio. A domain consists of an associated Amazon Elastic File System (EFS) volume, a list of authorized users, and a variety of security, application, policy, and Amazon Virtual Private Cloud (VPC) configurations. An AWS account is limited to one domain per region. Users within a domain can share notebook files and other artifacts with each other.
--
-- When a domain is created, an EFS volume is created for use by all of the users within the domain. Each user receives a private home directory within the EFS volume for notebooks, Git repositories, and data files.
-- __VPC configuration__ 
-- All SageMaker Studio traffic between the domain and the EFS volume is through the specified VPC and subnets. For other Studio traffic, you can specify the @AppNetworkAccessType@ parameter. @AppNetworkAccessType@ corresponds to the network access type that you choose when you onboard to Studio. The following options are available:
--
--     * @PublicInternetOnly@ - Non-EFS traffic goes through a VPC managed by Amazon SageMaker, which allows internet access. This is the default value.
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets. Internet access is disabled by default. To allow internet access, you must specify a NAT gateway.
-- When internet access is disabled, you won't be able to run a Studio notebook or to train or host models unless your VPC has an interface endpoint to the SageMaker API and runtime or a NAT gateway and your security groups allow outbound connections.
--
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-notebooks-and-internet-access.html Connect SageMaker Studio Notebooks to Resources in a VPC> .
module Network.AWS.SageMaker.CreateDomain
    (
    -- * Creating a request
      CreateDomain (..)
    , mkCreateDomain
    -- ** Request lenses
    , cdDomainName
    , cdAuthMode
    , cdDefaultUserSettings
    , cdSubnetIds
    , cdVpcId
    , cdAppNetworkAccessType
    , cdHomeEfsFileSystemKmsKeyId
    , cdKmsKeyId
    , cdTags

    -- * Destructuring the response
    , CreateDomainResponse (..)
    , mkCreateDomainResponse
    -- ** Response lenses
    , cdrrsDomainArn
    , cdrrsUrl
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { domainName :: Types.DomainName
    -- ^ A name for the domain.
  , authMode :: Types.AuthMode
    -- ^ The mode of authentication that members use to access the domain.
  , defaultUserSettings :: Types.UserSettings
    -- ^ The default user settings.
  , subnetIds :: Core.NonEmpty Types.SubnetId
    -- ^ The VPC subnets that Studio uses for communication.
  , vpcId :: Types.VpcId
    -- ^ The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
  , appNetworkAccessType :: Core.Maybe Types.AppNetworkAccessType
    -- ^ Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
--
--
--     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
--
--
--     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
--
--
  , homeEfsFileSystemKmsKeyId :: Core.Maybe Types.HomeEfsFileSystemKmsKeyId
    -- ^ This member is deprecated and replaced with @KmsKeyId@ .
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomain' value with any optional fields omitted.
mkCreateDomain
    :: Types.DomainName -- ^ 'domainName'
    -> Types.AuthMode -- ^ 'authMode'
    -> Types.UserSettings -- ^ 'defaultUserSettings'
    -> Core.NonEmpty Types.SubnetId -- ^ 'subnetIds'
    -> Types.VpcId -- ^ 'vpcId'
    -> CreateDomain
mkCreateDomain domainName authMode defaultUserSettings subnetIds
  vpcId
  = CreateDomain'{domainName, authMode, defaultUserSettings,
                  subnetIds, vpcId, appNetworkAccessType = Core.Nothing,
                  homeEfsFileSystemKmsKeyId = Core.Nothing, kmsKeyId = Core.Nothing,
                  tags = Core.Nothing}

-- | A name for the domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CreateDomain Types.DomainName
cdDomainName = Lens.field @"domainName"
{-# INLINEABLE cdDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The mode of authentication that members use to access the domain.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdAuthMode :: Lens.Lens' CreateDomain Types.AuthMode
cdAuthMode = Lens.field @"authMode"
{-# INLINEABLE cdAuthMode #-}
{-# DEPRECATED authMode "Use generic-lens or generic-optics with 'authMode' instead"  #-}

-- | The default user settings.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDefaultUserSettings :: Lens.Lens' CreateDomain Types.UserSettings
cdDefaultUserSettings = Lens.field @"defaultUserSettings"
{-# INLINEABLE cdDefaultUserSettings #-}
{-# DEPRECATED defaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead"  #-}

-- | The VPC subnets that Studio uses for communication.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubnetIds :: Lens.Lens' CreateDomain (Core.NonEmpty Types.SubnetId)
cdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE cdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdVpcId :: Lens.Lens' CreateDomain Types.VpcId
cdVpcId = Lens.field @"vpcId"
{-# INLINEABLE cdVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

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
cdAppNetworkAccessType :: Lens.Lens' CreateDomain (Core.Maybe Types.AppNetworkAccessType)
cdAppNetworkAccessType = Lens.field @"appNetworkAccessType"
{-# INLINEABLE cdAppNetworkAccessType #-}
{-# DEPRECATED appNetworkAccessType "Use generic-lens or generic-optics with 'appNetworkAccessType' instead"  #-}

-- | This member is deprecated and replaced with @KmsKeyId@ .
--
-- /Note:/ Consider using 'homeEfsFileSystemKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdHomeEfsFileSystemKmsKeyId :: Lens.Lens' CreateDomain (Core.Maybe Types.HomeEfsFileSystemKmsKeyId)
cdHomeEfsFileSystemKmsKeyId = Lens.field @"homeEfsFileSystemKmsKeyId"
{-# INLINEABLE cdHomeEfsFileSystemKmsKeyId #-}
{-# DEPRECATED homeEfsFileSystemKmsKeyId "Use generic-lens or generic-optics with 'homeEfsFileSystemKmsKeyId' instead"  #-}

-- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain with an AWS managed customer master key (CMK) by default. For more control, specify a customer managed CMK.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKmsKeyId :: Lens.Lens' CreateDomain (Core.Maybe Types.KmsKeyId)
cdKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE cdKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Tags to associated with the Domain. Each tag consists of a key and an optional value. Tag keys must be unique per resource. Tags are searchable using the 'Search' API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDomain (Core.Maybe [Types.Tag])
cdTags = Lens.field @"tags"
{-# INLINEABLE cdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDomain where
        toHeaders CreateDomain{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateDomain") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDomain where
        toJSON CreateDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainName" Core..= domainName),
                  Core.Just ("AuthMode" Core..= authMode),
                  Core.Just ("DefaultUserSettings" Core..= defaultUserSettings),
                  Core.Just ("SubnetIds" Core..= subnetIds),
                  Core.Just ("VpcId" Core..= vpcId),
                  ("AppNetworkAccessType" Core..=) Core.<$> appNetworkAccessType,
                  ("HomeEfsFileSystemKmsKeyId" Core..=) Core.<$>
                    homeEfsFileSystemKmsKeyId,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDomain where
        type Rs CreateDomain = CreateDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDomainResponse' Core.<$>
                   (x Core..:? "DomainArn") Core.<*> x Core..:? "Url" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { domainArn :: Core.Maybe Types.DomainArn
    -- ^ The Amazon Resource Name (ARN) of the created domain.
  , url :: Core.Maybe Types.Url
    -- ^ The URL to the created domain.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainResponse' value with any optional fields omitted.
mkCreateDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDomainResponse
mkCreateDomainResponse responseStatus
  = CreateDomainResponse'{domainArn = Core.Nothing,
                          url = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the created domain.
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsDomainArn :: Lens.Lens' CreateDomainResponse (Core.Maybe Types.DomainArn)
cdrrsDomainArn = Lens.field @"domainArn"
{-# INLINEABLE cdrrsDomainArn #-}
{-# DEPRECATED domainArn "Use generic-lens or generic-optics with 'domainArn' instead"  #-}

-- | The URL to the created domain.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsUrl :: Lens.Lens' CreateDomainResponse (Core.Maybe Types.Url)
cdrrsUrl = Lens.field @"url"
{-# INLINEABLE cdrrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' CreateDomainResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
