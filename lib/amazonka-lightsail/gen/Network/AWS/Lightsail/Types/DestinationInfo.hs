{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DestinationInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DestinationInfo
  ( DestinationInfo (..),

    -- * Smart constructor
    mkDestinationInfo,

    -- * Lenses
    diId,
    diService,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Id as Types
import qualified Network.AWS.Lightsail.Types.Service as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the destination of a record.
--
-- /See:/ 'mkDestinationInfo' smart constructor.
data DestinationInfo = DestinationInfo'
  { -- | The ID of the resource created at the destination.
    id :: Core.Maybe Types.Id,
    -- | The destination service of the record.
    service :: Core.Maybe Types.Service
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationInfo' value with any optional fields omitted.
mkDestinationInfo ::
  DestinationInfo
mkDestinationInfo =
  DestinationInfo' {id = Core.Nothing, service = Core.Nothing}

-- | The ID of the resource created at the destination.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diId :: Lens.Lens' DestinationInfo (Core.Maybe Types.Id)
diId = Lens.field @"id"
{-# DEPRECATED diId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The destination service of the record.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diService :: Lens.Lens' DestinationInfo (Core.Maybe Types.Service)
diService = Lens.field @"service"
{-# DEPRECATED diService "Use generic-lens or generic-optics with 'service' instead." #-}

instance Core.FromJSON DestinationInfo where
  parseJSON =
    Core.withObject "DestinationInfo" Core.$
      \x ->
        DestinationInfo'
          Core.<$> (x Core..:? "id") Core.<*> (x Core..:? "service")
