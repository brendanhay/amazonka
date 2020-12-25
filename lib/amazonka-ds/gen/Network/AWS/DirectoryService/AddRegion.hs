{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.AddRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds two domain controllers in the specified Region for the specified directory.
module Network.AWS.DirectoryService.AddRegion
  ( -- * Creating a request
    AddRegion (..),
    mkAddRegion,

    -- ** Request lenses
    arDirectoryId,
    arRegionName,
    arVPCSettings,

    -- * Destructuring the response
    AddRegionResponse (..),
    mkAddRegionResponse,

    -- ** Response lenses
    arrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddRegion' smart constructor.
data AddRegion = AddRegion'
  { -- | The identifier of the directory to which you want to add Region replication.
    directoryId :: Types.DirectoryId,
    -- | The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
    regionName :: Types.RegionName,
    vPCSettings :: Types.DirectoryVpcSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddRegion' value with any optional fields omitted.
mkAddRegion ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'regionName'
  Types.RegionName ->
  -- | 'vPCSettings'
  Types.DirectoryVpcSettings ->
  AddRegion
mkAddRegion directoryId regionName vPCSettings =
  AddRegion' {directoryId, regionName, vPCSettings}

-- | The identifier of the directory to which you want to add Region replication.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDirectoryId :: Lens.Lens' AddRegion Types.DirectoryId
arDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED arDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The name of the Region where you want to add domain controllers for replication. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRegionName :: Lens.Lens' AddRegion Types.RegionName
arRegionName = Lens.field @"regionName"
{-# DEPRECATED arRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vPCSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arVPCSettings :: Lens.Lens' AddRegion Types.DirectoryVpcSettings
arVPCSettings = Lens.field @"vPCSettings"
{-# DEPRECATED arVPCSettings "Use generic-lens or generic-optics with 'vPCSettings' instead." #-}

instance Core.FromJSON AddRegion where
  toJSON AddRegion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("RegionName" Core..= regionName),
            Core.Just ("VPCSettings" Core..= vPCSettings)
          ]
      )

instance Core.AWSRequest AddRegion where
  type Rs AddRegion = AddRegionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DirectoryService_20150416.AddRegion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddRegionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddRegionResponse' smart constructor.
newtype AddRegionResponse = AddRegionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddRegionResponse' value with any optional fields omitted.
mkAddRegionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddRegionResponse
mkAddRegionResponse responseStatus =
  AddRegionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrrsResponseStatus :: Lens.Lens' AddRegionResponse Core.Int
arrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED arrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
