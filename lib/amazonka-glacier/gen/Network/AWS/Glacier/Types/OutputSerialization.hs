{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.OutputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.OutputSerialization
  ( OutputSerialization (..)
  -- * Smart constructor
  , mkOutputSerialization
  -- * Lenses
  , osCsv
  ) where

import qualified Network.AWS.Glacier.Types.CSVOutput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes how the select output is serialized.
--
-- /See:/ 'mkOutputSerialization' smart constructor.
newtype OutputSerialization = OutputSerialization'
  { csv :: Core.Maybe Types.CSVOutput
    -- ^ Describes the serialization of CSV-encoded query results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputSerialization' value with any optional fields omitted.
mkOutputSerialization
    :: OutputSerialization
mkOutputSerialization = OutputSerialization'{csv = Core.Nothing}

-- | Describes the serialization of CSV-encoded query results.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCsv :: Lens.Lens' OutputSerialization (Core.Maybe Types.CSVOutput)
osCsv = Lens.field @"csv"
{-# INLINEABLE osCsv #-}
{-# DEPRECATED csv "Use generic-lens or generic-optics with 'csv' instead"  #-}

instance Core.FromJSON OutputSerialization where
        toJSON OutputSerialization{..}
          = Core.object (Core.catMaybes [("csv" Core..=) Core.<$> csv])

instance Core.FromJSON OutputSerialization where
        parseJSON
          = Core.withObject "OutputSerialization" Core.$
              \ x -> OutputSerialization' Core.<$> (x Core..:? "csv")
