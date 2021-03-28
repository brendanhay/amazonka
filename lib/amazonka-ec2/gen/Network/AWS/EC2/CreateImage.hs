{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that is either running or stopped.
--
-- If you customized your instance with instance store volumes or EBS volumes in addition to the root device volume, the new AMI contains block device mapping information for those volumes. When you launch an instance from this new AMI, the instance automatically launches with those additional volumes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating Amazon EBS-Backed Linux AMIs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateImage
    (
    -- * Creating a request
      CreateImage (..)
    , mkCreateImage
    -- ** Request lenses
    , cifInstanceId
    , cifName
    , cifBlockDeviceMappings
    , cifDescription
    , cifDryRun
    , cifNoReboot

    -- * Destructuring the response
    , CreateImageResponse (..)
    , mkCreateImageResponse
    -- ** Response lenses
    , cirfrsImageId
    , cirfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImage' smart constructor.
data CreateImage = CreateImage'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , name :: Core.Text
    -- ^ A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ The block device mappings. This parameter cannot be used to modify the encryption status of existing volumes or snapshots. To create an AMI with encrypted snapshots, use the 'CopyImage' action.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the new image.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , noReboot :: Core.Maybe Core.Bool
    -- ^ By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImage' value with any optional fields omitted.
mkCreateImage
    :: Types.InstanceId -- ^ 'instanceId'
    -> Core.Text -- ^ 'name'
    -> CreateImage
mkCreateImage instanceId name
  = CreateImage'{instanceId, name,
                 blockDeviceMappings = Core.Nothing, description = Core.Nothing,
                 dryRun = Core.Nothing, noReboot = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifInstanceId :: Lens.Lens' CreateImage Types.InstanceId
cifInstanceId = Lens.field @"instanceId"
{-# INLINEABLE cifInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifName :: Lens.Lens' CreateImage Core.Text
cifName = Lens.field @"name"
{-# INLINEABLE cifName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The block device mappings. This parameter cannot be used to modify the encryption status of existing volumes or snapshots. To create an AMI with encrypted snapshots, use the 'CopyImage' action.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifBlockDeviceMappings :: Lens.Lens' CreateImage (Core.Maybe [Types.BlockDeviceMapping])
cifBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE cifBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | A description for the new image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDescription :: Lens.Lens' CreateImage (Core.Maybe Core.Text)
cifDescription = Lens.field @"description"
{-# INLINEABLE cifDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDryRun :: Lens.Lens' CreateImage (Core.Maybe Core.Bool)
cifDryRun = Lens.field @"dryRun"
{-# INLINEABLE cifDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | By default, Amazon EC2 attempts to shut down and reboot the instance before creating the image. If the 'No Reboot' option is set, Amazon EC2 doesn't shut down the instance before creating the image. When this option is used, file system integrity on the created image can't be guaranteed.
--
-- /Note:/ Consider using 'noReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifNoReboot :: Lens.Lens' CreateImage (Core.Maybe Core.Bool)
cifNoReboot = Lens.field @"noReboot"
{-# INLINEABLE cifNoReboot #-}
{-# DEPRECATED noReboot "Use generic-lens or generic-optics with 'noReboot' instead"  #-}

instance Core.ToQuery CreateImage where
        toQuery CreateImage{..}
          = Core.toQueryPair "Action" ("CreateImage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "Name" name
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NoReboot") noReboot

instance Core.ToHeaders CreateImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateImage where
        type Rs CreateImage = CreateImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateImageResponse' Core.<$>
                   (x Core..@? "imageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { imageId :: Core.Maybe Core.Text
    -- ^ The ID of the new AMI.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageResponse' value with any optional fields omitted.
mkCreateImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateImageResponse
mkCreateImageResponse responseStatus
  = CreateImageResponse'{imageId = Core.Nothing, responseStatus}

-- | The ID of the new AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirfrsImageId :: Lens.Lens' CreateImageResponse (Core.Maybe Core.Text)
cirfrsImageId = Lens.field @"imageId"
{-# INLINEABLE cirfrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirfrsResponseStatus :: Lens.Lens' CreateImageResponse Core.Int
cirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
