{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.RecordFormat
  ( RecordFormat (..)
  -- * Smart constructor
  , mkRecordFormat
  -- * Lenses
  , rfRecordFormatType
  , rfMappingParameters
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.MappingParameters as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordFormatType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the record format and relevant mapping information that should be applied to schematize the records on the stream. 
--
-- /See:/ 'mkRecordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { recordFormatType :: Types.RecordFormatType
    -- ^ The type of record format.
  , mappingParameters :: Core.Maybe Types.MappingParameters
    -- ^ When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordFormat' value with any optional fields omitted.
mkRecordFormat
    :: Types.RecordFormatType -- ^ 'recordFormatType'
    -> RecordFormat
mkRecordFormat recordFormatType
  = RecordFormat'{recordFormatType, mappingParameters = Core.Nothing}

-- | The type of record format.
--
-- /Note:/ Consider using 'recordFormatType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfRecordFormatType :: Lens.Lens' RecordFormat Types.RecordFormatType
rfRecordFormatType = Lens.field @"recordFormatType"
{-# INLINEABLE rfRecordFormatType #-}
{-# DEPRECATED recordFormatType "Use generic-lens or generic-optics with 'recordFormatType' instead"  #-}

-- | When configuring application input at the time of creating or updating an application, provides additional mapping information specific to the record format (such as JSON, CSV, or record fields delimited by some delimiter) on the streaming source.
--
-- /Note:/ Consider using 'mappingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfMappingParameters :: Lens.Lens' RecordFormat (Core.Maybe Types.MappingParameters)
rfMappingParameters = Lens.field @"mappingParameters"
{-# INLINEABLE rfMappingParameters #-}
{-# DEPRECATED mappingParameters "Use generic-lens or generic-optics with 'mappingParameters' instead"  #-}

instance Core.FromJSON RecordFormat where
        toJSON RecordFormat{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RecordFormatType" Core..= recordFormatType),
                  ("MappingParameters" Core..=) Core.<$> mappingParameters])

instance Core.FromJSON RecordFormat where
        parseJSON
          = Core.withObject "RecordFormat" Core.$
              \ x ->
                RecordFormat' Core.<$>
                  (x Core..: "RecordFormatType") Core.<*>
                    x Core..:? "MappingParameters"
