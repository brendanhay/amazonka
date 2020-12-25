{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an interconnect between an AWS Direct Connect Partner's network and a specific AWS Direct Connect location.
--
-- An interconnect is a connection that is capable of hosting other connections. The AWS Direct Connect partner can use an interconnect to provide AWS Direct Connect hosted connections to customers through their own network services. Like a standard connection, an interconnect links the partner's network to an AWS Direct Connect location over a standard Ethernet fiber-optic cable. One end is connected to the partner's router, the other to an AWS Direct Connect router.
-- You can automatically add the new interconnect to a link aggregation group (LAG) by specifying a LAG ID in the request. This ensures that the new interconnect is allocated on the same AWS Direct Connect endpoint that hosts the specified LAG. If there are no available ports on the endpoint, the request fails and no interconnect is created.
-- For each end customer, the AWS Direct Connect Partner provisions a connection on their interconnect by calling 'AllocateHostedConnection' . The end customer can then connect to AWS resources by creating a virtual interface on their connection, using the VLAN assigned to them by the AWS Direct Connect Partner.
module Network.AWS.DirectConnect.CreateInterconnect
  ( -- * Creating a request
    CreateInterconnect (..),
    mkCreateInterconnect,

    -- ** Request lenses
    ciInterconnectName,
    ciBandwidth,
    ciLocation,
    ciLagId,
    ciProviderName,
    ciTags,

    -- * Destructuring the response
    Types.Interconnect (..),
    Types.mkInterconnect,

    -- ** Response lenses
    Types.iAwsDevice,
    Types.iAwsDeviceV2,
    Types.iBandwidth,
    Types.iHasLogicalRedundancy,
    Types.iInterconnectId,
    Types.iInterconnectName,
    Types.iInterconnectState,
    Types.iJumboFrameCapable,
    Types.iLagId,
    Types.iLoaIssueTime,
    Types.iLocation,
    Types.iProviderName,
    Types.iRegion,
    Types.iTags,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInterconnect' smart constructor.
data CreateInterconnect = CreateInterconnect'
  { -- | The name of the interconnect.
    interconnectName :: Types.InterconnectName,
    -- | The port bandwidth, in Gbps. The possible values are 1 and 10.
    bandwidth :: Types.Bandwidth,
    -- | The location of the interconnect.
    location :: Types.LocationCode,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Types.LagId,
    -- | The name of the service provider associated with the interconnect.
    providerName :: Core.Maybe Types.ProviderName,
    -- | The tags to associate with the interconnect.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInterconnect' value with any optional fields omitted.
mkCreateInterconnect ::
  -- | 'interconnectName'
  Types.InterconnectName ->
  -- | 'bandwidth'
  Types.Bandwidth ->
  -- | 'location'
  Types.LocationCode ->
  CreateInterconnect
mkCreateInterconnect interconnectName bandwidth location =
  CreateInterconnect'
    { interconnectName,
      bandwidth,
      location,
      lagId = Core.Nothing,
      providerName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the interconnect.
--
-- /Note:/ Consider using 'interconnectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInterconnectName :: Lens.Lens' CreateInterconnect Types.InterconnectName
ciInterconnectName = Lens.field @"interconnectName"
{-# DEPRECATED ciInterconnectName "Use generic-lens or generic-optics with 'interconnectName' instead." #-}

-- | The port bandwidth, in Gbps. The possible values are 1 and 10.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBandwidth :: Lens.Lens' CreateInterconnect Types.Bandwidth
ciBandwidth = Lens.field @"bandwidth"
{-# DEPRECATED ciBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | The location of the interconnect.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLocation :: Lens.Lens' CreateInterconnect Types.LocationCode
ciLocation = Lens.field @"location"
{-# DEPRECATED ciLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLagId :: Lens.Lens' CreateInterconnect (Core.Maybe Types.LagId)
ciLagId = Lens.field @"lagId"
{-# DEPRECATED ciLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The name of the service provider associated with the interconnect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciProviderName :: Lens.Lens' CreateInterconnect (Core.Maybe Types.ProviderName)
ciProviderName = Lens.field @"providerName"
{-# DEPRECATED ciProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The tags to associate with the interconnect.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInterconnect (Core.Maybe (Core.NonEmpty Types.Tag))
ciTags = Lens.field @"tags"
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateInterconnect where
  toJSON CreateInterconnect {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("interconnectName" Core..= interconnectName),
            Core.Just ("bandwidth" Core..= bandwidth),
            Core.Just ("location" Core..= location),
            ("lagId" Core..=) Core.<$> lagId,
            ("providerName" Core..=) Core.<$> providerName,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateInterconnect where
  type Rs CreateInterconnect = Types.Interconnect
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OvertureService.CreateInterconnect")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
