{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
  ( InputSchemaUpdate (..)
  -- * Smart constructor
  , mkInputSchemaUpdate
  -- * Lenses
  , isuRecordColumnUpdates
  , isuRecordEncodingUpdate
  , isuRecordFormatUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.RecordColumn as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordEncoding as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordFormat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes updates for the application's input schema.
--
-- /See:/ 'mkInputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { recordColumnUpdates :: Core.Maybe (Core.NonEmpty Types.RecordColumn)
    -- ^ A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream. 
  , recordEncodingUpdate :: Core.Maybe Types.RecordEncoding
    -- ^ Specifies the encoding of the records in the streaming source. For example, UTF-8.
  , recordFormatUpdate :: Core.Maybe Types.RecordFormat
    -- ^ Specifies the format of the records on the streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSchemaUpdate' value with any optional fields omitted.
mkInputSchemaUpdate
    :: InputSchemaUpdate
mkInputSchemaUpdate
  = InputSchemaUpdate'{recordColumnUpdates = Core.Nothing,
                       recordEncodingUpdate = Core.Nothing,
                       recordFormatUpdate = Core.Nothing}

-- | A list of @RecordColumn@ objects. Each object describes the mapping of the streaming source element to the corresponding column in the in-application stream. 
--
-- /Note:/ Consider using 'recordColumnUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordColumnUpdates :: Lens.Lens' InputSchemaUpdate (Core.Maybe (Core.NonEmpty Types.RecordColumn))
isuRecordColumnUpdates = Lens.field @"recordColumnUpdates"
{-# INLINEABLE isuRecordColumnUpdates #-}
{-# DEPRECATED recordColumnUpdates "Use generic-lens or generic-optics with 'recordColumnUpdates' instead"  #-}

-- | Specifies the encoding of the records in the streaming source. For example, UTF-8.
--
-- /Note:/ Consider using 'recordEncodingUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordEncodingUpdate :: Lens.Lens' InputSchemaUpdate (Core.Maybe Types.RecordEncoding)
isuRecordEncodingUpdate = Lens.field @"recordEncodingUpdate"
{-# INLINEABLE isuRecordEncodingUpdate #-}
{-# DEPRECATED recordEncodingUpdate "Use generic-lens or generic-optics with 'recordEncodingUpdate' instead"  #-}

-- | Specifies the format of the records on the streaming source.
--
-- /Note:/ Consider using 'recordFormatUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuRecordFormatUpdate :: Lens.Lens' InputSchemaUpdate (Core.Maybe Types.RecordFormat)
isuRecordFormatUpdate = Lens.field @"recordFormatUpdate"
{-# INLINEABLE isuRecordFormatUpdate #-}
{-# DEPRECATED recordFormatUpdate "Use generic-lens or generic-optics with 'recordFormatUpdate' instead"  #-}

instance Core.FromJSON InputSchemaUpdate where
        toJSON InputSchemaUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("RecordColumnUpdates" Core..=) Core.<$> recordColumnUpdates,
                  ("RecordEncodingUpdate" Core..=) Core.<$> recordEncodingUpdate,
                  ("RecordFormatUpdate" Core..=) Core.<$> recordFormatUpdate])
