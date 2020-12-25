{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventorySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventorySchedule
  ( InventorySchedule (..),

    -- * Smart constructor
    mkInventorySchedule,

    -- * Lenses
    isFrequency,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.InventoryFrequency as Types

-- | Specifies the schedule for generating inventory results.
--
-- /See:/ 'mkInventorySchedule' smart constructor.
newtype InventorySchedule = InventorySchedule'
  { -- | Specifies how frequently inventory results are produced.
    frequency :: Types.InventoryFrequency
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InventorySchedule' value with any optional fields omitted.
mkInventorySchedule ::
  -- | 'frequency'
  Types.InventoryFrequency ->
  InventorySchedule
mkInventorySchedule frequency = InventorySchedule' {frequency}

-- | Specifies how frequently inventory results are produced.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFrequency :: Lens.Lens' InventorySchedule Types.InventoryFrequency
isFrequency = Lens.field @"frequency"
{-# DEPRECATED isFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

instance Core.ToXML InventorySchedule where
  toXML InventorySchedule {..} = Core.toXMLNode "Frequency" frequency

instance Core.FromXML InventorySchedule where
  parseXML x = InventorySchedule' Core.<$> (x Core..@ "Frequency")
