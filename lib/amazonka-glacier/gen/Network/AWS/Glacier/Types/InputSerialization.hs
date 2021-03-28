{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InputSerialization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.InputSerialization
  ( InputSerialization (..)
  -- * Smart constructor
  , mkInputSerialization
  -- * Lenses
  , isCsv
  ) where

import qualified Network.AWS.Glacier.Types.CSVInput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes how the archive is serialized.
--
-- /See:/ 'mkInputSerialization' smart constructor.
newtype InputSerialization = InputSerialization'
  { csv :: Core.Maybe Types.CSVInput
    -- ^ Describes the serialization of a CSV-encoded object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputSerialization' value with any optional fields omitted.
mkInputSerialization
    :: InputSerialization
mkInputSerialization = InputSerialization'{csv = Core.Nothing}

-- | Describes the serialization of a CSV-encoded object.
--
-- /Note:/ Consider using 'csv' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCsv :: Lens.Lens' InputSerialization (Core.Maybe Types.CSVInput)
isCsv = Lens.field @"csv"
{-# INLINEABLE isCsv #-}
{-# DEPRECATED csv "Use generic-lens or generic-optics with 'csv' instead"  #-}

instance Core.FromJSON InputSerialization where
        toJSON InputSerialization{..}
          = Core.object (Core.catMaybes [("csv" Core..=) Core.<$> csv])

instance Core.FromJSON InputSerialization where
        parseJSON
          = Core.withObject "InputSerialization" Core.$
              \ x -> InputSerialization' Core.<$> (x Core..:? "csv")
