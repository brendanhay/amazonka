{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk that can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
-- The @create disk@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDisk
  ( -- * Creating a request
    CreateDisk (..),
    mkCreateDisk,

    -- ** Request lenses
    cdgDiskName,
    cdgAvailabilityZone,
    cdgSizeInGb,
    cdgAddOns,
    cdgTags,

    -- * Destructuring the response
    CreateDiskResponse (..),
    mkCreateDiskResponse,

    -- ** Response lenses
    cdrfrsOperations,
    cdrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDisk' smart constructor.
data CreateDisk = CreateDisk'
  { -- | The unique Lightsail disk name (e.g., @my-disk@ ).
    diskName :: Types.ResourceName,
    -- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk.
    --
    -- Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
    availabilityZone :: Types.NonEmptyString,
    -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Core.Int,
    -- | An array of objects that represent the add-ons to enable for the new disk.
    addOns :: Core.Maybe [Types.AddOnRequest],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDisk' value with any optional fields omitted.
mkCreateDisk ::
  -- | 'diskName'
  Types.ResourceName ->
  -- | 'availabilityZone'
  Types.NonEmptyString ->
  -- | 'sizeInGb'
  Core.Int ->
  CreateDisk
mkCreateDisk diskName availabilityZone sizeInGb =
  CreateDisk'
    { diskName,
      availabilityZone,
      sizeInGb,
      addOns = Core.Nothing,
      tags = Core.Nothing
    }

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgDiskName :: Lens.Lens' CreateDisk Types.ResourceName
cdgDiskName = Lens.field @"diskName"
{-# DEPRECATED cdgDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk.
--
-- Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAvailabilityZone :: Lens.Lens' CreateDisk Types.NonEmptyString
cdgAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED cdgAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgSizeInGb :: Lens.Lens' CreateDisk Core.Int
cdgSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED cdgSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | An array of objects that represent the add-ons to enable for the new disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgAddOns :: Lens.Lens' CreateDisk (Core.Maybe [Types.AddOnRequest])
cdgAddOns = Lens.field @"addOns"
{-# DEPRECATED cdgAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdgTags :: Lens.Lens' CreateDisk (Core.Maybe [Types.Tag])
cdgTags = Lens.field @"tags"
{-# DEPRECATED cdgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateDisk where
  toJSON CreateDisk {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("diskName" Core..= diskName),
            Core.Just ("availabilityZone" Core..= availabilityZone),
            Core.Just ("sizeInGb" Core..= sizeInGb),
            ("addOns" Core..=) Core.<$> addOns,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateDisk where
  type Rs CreateDisk = CreateDiskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.CreateDisk")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDiskResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDiskResponse' smart constructor.
data CreateDiskResponse = CreateDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDiskResponse' value with any optional fields omitted.
mkCreateDiskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDiskResponse
mkCreateDiskResponse responseStatus =
  CreateDiskResponse' {operations = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrfrsOperations :: Lens.Lens' CreateDiskResponse (Core.Maybe [Types.Operation])
cdrfrsOperations = Lens.field @"operations"
{-# DEPRECATED cdrfrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrfrsResponseStatus :: Lens.Lens' CreateDiskResponse Core.Int
cdrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
