{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeDomain (..),
    mkDescribeDomain,

    -- ** Request lenses
    ddfDomainId,

    -- * Destructuring the response
    DescribeDomainResponse (..),
    mkDescribeDomainResponse,

    -- ** Response lenses
    ddrrsAppNetworkAccessType,
    ddrrsAuthMode,
    ddrrsCreationTime,
    ddrrsDefaultUserSettings,
    ddrrsDomainArn,
    ddrrsDomainId,
    ddrrsDomainName,
    ddrrsFailureReason,
    ddrrsHomeEfsFileSystemId,
    ddrrsHomeEfsFileSystemKmsKeyId,
    ddrrsKmsKeyId,
    ddrrsLastModifiedTime,
    ddrrsSingleSignOnManagedApplicationInstanceId,
    ddrrsStatus,
    ddrrsSubnetIds,
    ddrrsUrl,
    ddrrsVpcId,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeDomain' smart constructor.
newtype DescribeDomain = DescribeDomain'
  { -- | The domain ID.
    domainId :: Types.DomainId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDomain' value with any optional fields omitted.
mkDescribeDomain ::
  -- | 'domainId'
  Types.DomainId ->
  DescribeDomain
mkDescribeDomain domainId = DescribeDomain' {domainId}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDomainId :: Lens.Lens' DescribeDomain Types.DomainId
ddfDomainId = Lens.field @"domainId"
{-# DEPRECATED ddfDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Core.FromJSON DescribeDomain where
  toJSON DescribeDomain {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DomainId" Core..= domainId)])

instance Core.AWSRequest DescribeDomain where
  type Rs DescribeDomain = DescribeDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Core.<$> (x Core..:? "AppNetworkAccessType")
            Core.<*> (x Core..:? "AuthMode")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "DefaultUserSettings")
            Core.<*> (x Core..:? "DomainArn")
            Core.<*> (x Core..:? "DomainId")
            Core.<*> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "HomeEfsFileSystemId")
            Core.<*> (x Core..:? "HomeEfsFileSystemKmsKeyId")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "SingleSignOnManagedApplicationInstanceId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "SubnetIds")
            Core.<*> (x Core..:? "Url")
            Core.<*> (x Core..:? "VpcId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | Specifies the VPC used for non-EFS traffic. The default value is @PublicInternetOnly@ .
    --
    --
    --     * @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by Amazon SageMaker, which allows direct internet access
    --
    --
    --     * @VpcOnly@ - All Studio traffic is through the specified VPC and subnets
    appNetworkAccessType :: Core.Maybe Types.AppNetworkAccessType,
    -- | The domain's authentication mode.
    authMode :: Core.Maybe Types.AuthMode,
    -- | The creation time.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
    defaultUserSettings :: Core.Maybe Types.UserSettings,
    -- | The domain's Amazon Resource Name (ARN).
    domainArn :: Core.Maybe Types.DomainArn,
    -- | The domain ID.
    domainId :: Core.Maybe Types.DomainId,
    -- | The domain name.
    domainName :: Core.Maybe Types.DomainName,
    -- | The failure reason.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
    homeEfsFileSystemId :: Core.Maybe Types.ResourceId,
    -- | This member is deprecated and replaced with @KmsKeyId@ .
    homeEfsFileSystemKmsKeyId :: Core.Maybe Types.HomeEfsFileSystemKmsKeyId,
    -- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The SSO managed application instance ID.
    singleSignOnManagedApplicationInstanceId :: Core.Maybe Types.String256,
    -- | The status.
    status :: Core.Maybe Types.DomainStatus,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Core.Maybe (Core.NonEmpty Types.SubnetId),
    -- | The domain's URL.
    url :: Core.Maybe Types.Url,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
    vpcId :: Core.Maybe Types.VpcId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDomainResponse' value with any optional fields omitted.
mkDescribeDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDomainResponse
mkDescribeDomainResponse responseStatus =
  DescribeDomainResponse'
    { appNetworkAccessType = Core.Nothing,
      authMode = Core.Nothing,
      creationTime = Core.Nothing,
      defaultUserSettings = Core.Nothing,
      domainArn = Core.Nothing,
      domainId = Core.Nothing,
      domainName = Core.Nothing,
      failureReason = Core.Nothing,
      homeEfsFileSystemId = Core.Nothing,
      homeEfsFileSystemKmsKeyId = Core.Nothing,
      kmsKeyId = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      singleSignOnManagedApplicationInstanceId = Core.Nothing,
      status = Core.Nothing,
      subnetIds = Core.Nothing,
      url = Core.Nothing,
      vpcId = Core.Nothing,
      responseStatus
    }

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
{-# DEPRECATED ddrrsAppNetworkAccessType "Use generic-lens or generic-optics with 'appNetworkAccessType' instead." #-}

-- | The domain's authentication mode.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsAuthMode :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.AuthMode)
ddrrsAuthMode = Lens.field @"authMode"
{-# DEPRECATED ddrrsAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsCreationTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.NominalDiffTime)
ddrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ddrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Settings which are applied to all UserProfiles in this domain, if settings are not explicitly specified in a given UserProfile.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDefaultUserSettings :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.UserSettings)
ddrrsDefaultUserSettings = Lens.field @"defaultUserSettings"
{-# DEPRECATED ddrrsDefaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead." #-}

-- | The domain's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainArn :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainArn)
ddrrsDomainArn = Lens.field @"domainArn"
{-# DEPRECATED ddrrsDomainArn "Use generic-lens or generic-optics with 'domainArn' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainId)
ddrrsDomainId = Lens.field @"domainId"
{-# DEPRECATED ddrrsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDomainName :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainName)
ddrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED ddrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsFailureReason :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.FailureReason)
ddrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED ddrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- /Note:/ Consider using 'homeEfsFileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsHomeEfsFileSystemId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.ResourceId)
ddrrsHomeEfsFileSystemId = Lens.field @"homeEfsFileSystemId"
{-# DEPRECATED ddrrsHomeEfsFileSystemId "Use generic-lens or generic-optics with 'homeEfsFileSystemId' instead." #-}

-- | This member is deprecated and replaced with @KmsKeyId@ .
--
-- /Note:/ Consider using 'homeEfsFileSystemKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsHomeEfsFileSystemKmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.HomeEfsFileSystemKmsKeyId)
ddrrsHomeEfsFileSystemKmsKeyId = Lens.field @"homeEfsFileSystemKmsKeyId"
{-# DEPRECATED ddrrsHomeEfsFileSystemKmsKeyId "Use generic-lens or generic-optics with 'homeEfsFileSystemKmsKeyId' instead." #-}

-- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached to the domain.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsKmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.KmsKeyId)
ddrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ddrrsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsLastModifiedTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.NominalDiffTime)
ddrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED ddrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The SSO managed application instance ID.
--
-- /Note:/ Consider using 'singleSignOnManagedApplicationInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsSingleSignOnManagedApplicationInstanceId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.String256)
ddrrsSingleSignOnManagedApplicationInstanceId = Lens.field @"singleSignOnManagedApplicationInstanceId"
{-# DEPRECATED ddrrsSingleSignOnManagedApplicationInstanceId "Use generic-lens or generic-optics with 'singleSignOnManagedApplicationInstanceId' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsStatus :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.DomainStatus)
ddrrsStatus = Lens.field @"status"
{-# DEPRECATED ddrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The VPC subnets that Studio uses for communication.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsSubnetIds :: Lens.Lens' DescribeDomainResponse (Core.Maybe (Core.NonEmpty Types.SubnetId))
ddrrsSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED ddrrsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The domain's URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsUrl :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.Url)
ddrrsUrl = Lens.field @"url"
{-# DEPRECATED ddrrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for communication.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsVpcId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Types.VpcId)
ddrrsVpcId = Lens.field @"vpcId"
{-# DEPRECATED ddrrsVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDomainResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
