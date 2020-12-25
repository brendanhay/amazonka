{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dDestinationId,
    dDestinationType,
    dStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types.DestinationId as Types
import qualified Network.AWS.GuardDuty.Types.DestinationType as Types
import qualified Network.AWS.GuardDuty.Types.PublishingStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the publishing destination, including the ID, type, and status.
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { -- | The unique ID of the publishing destination.
    destinationId :: Types.DestinationId,
    -- | The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
    destinationType :: Types.DestinationType,
    -- | The status of the publishing destination.
    status :: Types.PublishingStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Destination' value with any optional fields omitted.
mkDestination ::
  -- | 'destinationId'
  Types.DestinationId ->
  -- | 'destinationType'
  Types.DestinationType ->
  -- | 'status'
  Types.PublishingStatus ->
  Destination
mkDestination destinationId destinationType status =
  Destination' {destinationId, destinationType, status}

-- | The unique ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationId :: Lens.Lens' Destination Types.DestinationId
dDestinationId = Lens.field @"destinationId"
{-# DEPRECATED dDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

-- | The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationType :: Lens.Lens' Destination Types.DestinationType
dDestinationType = Lens.field @"destinationType"
{-# DEPRECATED dDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Destination Types.PublishingStatus
dStatus = Lens.field @"status"
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON Destination where
  parseJSON =
    Core.withObject "Destination" Core.$
      \x ->
        Destination'
          Core.<$> (x Core..: "destinationId")
          Core.<*> (x Core..: "destinationType")
          Core.<*> (x Core..: "status")
