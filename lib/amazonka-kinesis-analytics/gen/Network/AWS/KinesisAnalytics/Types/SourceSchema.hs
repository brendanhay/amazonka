{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.SourceSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.SourceSchema
  ( SourceSchema (..)
  -- * Smart constructor
  , mkSourceSchema
  -- * Lenses
  , ssRecordFormat
  , ssRecordColumns
  , ssRecordEncoding
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.RecordColumn as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordEncoding as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /See:/ 'mkSourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { recordFormat :: Types.RecordFormat
    -- ^ Specifies the format of the records on the streaming source.
  , recordColumns :: Core.NonEmpty Types.RecordColumn
    -- ^ A list of @RecordColumn@ objects.
  , recordEncoding :: Core.Maybe Types.RecordEncoding
    -- ^ Specifies the encoding of the records in the streaming source. For example, UTF-8.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceSchema' value with any optional fields omitted.
mkSourceSchema
    :: Types.RecordFormat -- ^ 'recordFormat'
    -> Core.NonEmpty Types.RecordColumn -- ^ 'recordColumns'
    -> SourceSchema
mkSourceSchema recordFormat recordColumns
  = SourceSchema'{recordFormat, recordColumns,
                  recordEncoding = Core.Nothing}

-- | Specifies the format of the records on the streaming source.
--
-- /Note:/ Consider using 'recordFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordFormat :: Lens.Lens' SourceSchema Types.RecordFormat
ssRecordFormat = Lens.field @"recordFormat"
{-# INLINEABLE ssRecordFormat #-}
{-# DEPRECATED recordFormat "Use generic-lens or generic-optics with 'recordFormat' instead"  #-}

-- | A list of @RecordColumn@ objects.
--
-- /Note:/ Consider using 'recordColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordColumns :: Lens.Lens' SourceSchema (Core.NonEmpty Types.RecordColumn)
ssRecordColumns = Lens.field @"recordColumns"
{-# INLINEABLE ssRecordColumns #-}
{-# DEPRECATED recordColumns "Use generic-lens or generic-optics with 'recordColumns' instead"  #-}

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- /Note:/ Consider using 'recordEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRecordEncoding :: Lens.Lens' SourceSchema (Core.Maybe Types.RecordEncoding)
ssRecordEncoding = Lens.field @"recordEncoding"
{-# INLINEABLE ssRecordEncoding #-}
{-# DEPRECATED recordEncoding "Use generic-lens or generic-optics with 'recordEncoding' instead"  #-}

instance Core.FromJSON SourceSchema where
        toJSON SourceSchema{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RecordFormat" Core..= recordFormat),
                  Core.Just ("RecordColumns" Core..= recordColumns),
                  ("RecordEncoding" Core..=) Core.<$> recordEncoding])

instance Core.FromJSON SourceSchema where
        parseJSON
          = Core.withObject "SourceSchema" Core.$
              \ x ->
                SourceSchema' Core.<$>
                  (x Core..: "RecordFormat") Core.<*> x Core..: "RecordColumns"
                    Core.<*> x Core..:? "RecordEncoding"
