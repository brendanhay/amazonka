{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified directory. This operation is asynchronous and returns before the WorkSpace directory is registered. If this is the first time you are registering a directory, you will need to create the workspaces_DefaultRole role before you can register a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role> .
module Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
    (
    -- * Creating a request
      RegisterWorkspaceDirectory (..)
    , mkRegisterWorkspaceDirectory
    -- ** Request lenses
    , rwdDirectoryId
    , rwdEnableWorkDocs
    , rwdEnableSelfService
    , rwdSubnetIds
    , rwdTags
    , rwdTenancy

    -- * Destructuring the response
    , RegisterWorkspaceDirectoryResponse (..)
    , mkRegisterWorkspaceDirectoryResponse
    -- ** Response lenses
    , rwdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRegisterWorkspaceDirectory' smart constructor.
data RegisterWorkspaceDirectory = RegisterWorkspaceDirectory'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
  , enableWorkDocs :: Core.Bool
    -- ^ Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
  , enableSelfService :: Core.Maybe Core.Bool
    -- ^ Indicates whether self-service capabilities are enabled or disabled.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the directory.
  , tenancy :: Core.Maybe Types.Tenancy
    -- ^ Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWorkspaceDirectory' value with any optional fields omitted.
mkRegisterWorkspaceDirectory
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Core.Bool -- ^ 'enableWorkDocs'
    -> RegisterWorkspaceDirectory
mkRegisterWorkspaceDirectory directoryId enableWorkDocs
  = RegisterWorkspaceDirectory'{directoryId, enableWorkDocs,
                                enableSelfService = Core.Nothing, subnetIds = Core.Nothing,
                                tags = Core.Nothing, tenancy = Core.Nothing}

-- | The identifier of the directory. You cannot register a directory if it does not have a status of Active. If the directory does not have a status of Active, you will receive an InvalidResourceStateException error. If you have already registered the maximum number of directories that you can register with Amazon WorkSpaces, you will receive a ResourceLimitExceededException error. Deregister directories that you are not using for WorkSpaces, and try again.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdDirectoryId :: Lens.Lens' RegisterWorkspaceDirectory Types.DirectoryId
rwdDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE rwdDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | Indicates whether Amazon WorkDocs is enabled or disabled. If you have enabled this parameter and WorkDocs is not available in the Region, you will receive an OperationNotSupportedException error. Set @EnableWorkDocs@ to disabled, and try again.
--
-- /Note:/ Consider using 'enableWorkDocs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdEnableWorkDocs :: Lens.Lens' RegisterWorkspaceDirectory Core.Bool
rwdEnableWorkDocs = Lens.field @"enableWorkDocs"
{-# INLINEABLE rwdEnableWorkDocs #-}
{-# DEPRECATED enableWorkDocs "Use generic-lens or generic-optics with 'enableWorkDocs' instead"  #-}

-- | Indicates whether self-service capabilities are enabled or disabled.
--
-- /Note:/ Consider using 'enableSelfService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdEnableSelfService :: Lens.Lens' RegisterWorkspaceDirectory (Core.Maybe Core.Bool)
rwdEnableSelfService = Lens.field @"enableSelfService"
{-# INLINEABLE rwdEnableSelfService #-}
{-# DEPRECATED enableSelfService "Use generic-lens or generic-optics with 'enableSelfService' instead"  #-}

-- | The identifiers of the subnets for your virtual private cloud (VPC). Make sure that the subnets are in supported Availability Zones. The subnets must also be in separate Availability Zones. If these conditions are not met, you will receive an OperationNotSupportedException error.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdSubnetIds :: Lens.Lens' RegisterWorkspaceDirectory (Core.Maybe [Types.SubnetId])
rwdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE rwdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The tags associated with the directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdTags :: Lens.Lens' RegisterWorkspaceDirectory (Core.Maybe [Types.Tag])
rwdTags = Lens.field @"tags"
{-# INLINEABLE rwdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Indicates whether your WorkSpace directory is dedicated or shared. To use Bring Your Own License (BYOL) images, this value must be set to @DEDICATED@ and your AWS account must be enabled for BYOL. If your account has not been enabled for BYOL, you will receive an InvalidParameterValuesException error. For more information about BYOL images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdTenancy :: Lens.Lens' RegisterWorkspaceDirectory (Core.Maybe Types.Tenancy)
rwdTenancy = Lens.field @"tenancy"
{-# INLINEABLE rwdTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

instance Core.ToQuery RegisterWorkspaceDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterWorkspaceDirectory where
        toHeaders RegisterWorkspaceDirectory{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.RegisterWorkspaceDirectory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterWorkspaceDirectory where
        toJSON RegisterWorkspaceDirectory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("EnableWorkDocs" Core..= enableWorkDocs),
                  ("EnableSelfService" Core..=) Core.<$> enableSelfService,
                  ("SubnetIds" Core..=) Core.<$> subnetIds,
                  ("Tags" Core..=) Core.<$> tags,
                  ("Tenancy" Core..=) Core.<$> tenancy])

instance Core.AWSRequest RegisterWorkspaceDirectory where
        type Rs RegisterWorkspaceDirectory =
             RegisterWorkspaceDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RegisterWorkspaceDirectoryResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterWorkspaceDirectoryResponse' smart constructor.
newtype RegisterWorkspaceDirectoryResponse = RegisterWorkspaceDirectoryResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWorkspaceDirectoryResponse' value with any optional fields omitted.
mkRegisterWorkspaceDirectoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterWorkspaceDirectoryResponse
mkRegisterWorkspaceDirectoryResponse responseStatus
  = RegisterWorkspaceDirectoryResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwdrrsResponseStatus :: Lens.Lens' RegisterWorkspaceDirectoryResponse Core.Int
rwdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rwdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
