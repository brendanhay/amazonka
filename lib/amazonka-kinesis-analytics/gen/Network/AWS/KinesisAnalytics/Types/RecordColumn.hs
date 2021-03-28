{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordColumn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.RecordColumn
  ( RecordColumn (..)
  -- * Smart constructor
  , mkRecordColumn
  -- * Lenses
  , rcName
  , rcSqlType
  , rcMapping
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.RecordColumnMapping as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordColumnName as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordColumnSqlType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the mapping of each data element in the streaming source to the corresponding column in the in-application stream.
--
-- Also used to describe the format of the reference data source.
--
-- /See:/ 'mkRecordColumn' smart constructor.
data RecordColumn = RecordColumn'
  { name :: Types.RecordColumnName
    -- ^ Name of the column created in the in-application input stream or reference table.
  , sqlType :: Types.RecordColumnSqlType
    -- ^ Type of column created in the in-application input stream or reference table.
  , mapping :: Core.Maybe Types.RecordColumnMapping
    -- ^ Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordColumn' value with any optional fields omitted.
mkRecordColumn
    :: Types.RecordColumnName -- ^ 'name'
    -> Types.RecordColumnSqlType -- ^ 'sqlType'
    -> RecordColumn
mkRecordColumn name sqlType
  = RecordColumn'{name, sqlType, mapping = Core.Nothing}

-- | Name of the column created in the in-application input stream or reference table.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcName :: Lens.Lens' RecordColumn Types.RecordColumnName
rcName = Lens.field @"name"
{-# INLINEABLE rcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Type of column created in the in-application input stream or reference table.
--
-- /Note:/ Consider using 'sqlType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSqlType :: Lens.Lens' RecordColumn Types.RecordColumnSqlType
rcSqlType = Lens.field @"sqlType"
{-# INLINEABLE rcSqlType #-}
{-# DEPRECATED sqlType "Use generic-lens or generic-optics with 'sqlType' instead"  #-}

-- | Reference to the data element in the streaming input or the reference data source. This element is required if the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType> is @JSON@ .
--
-- /Note:/ Consider using 'mapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMapping :: Lens.Lens' RecordColumn (Core.Maybe Types.RecordColumnMapping)
rcMapping = Lens.field @"mapping"
{-# INLINEABLE rcMapping #-}
{-# DEPRECATED mapping "Use generic-lens or generic-optics with 'mapping' instead"  #-}

instance Core.FromJSON RecordColumn where
        toJSON RecordColumn{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("SqlType" Core..= sqlType),
                  ("Mapping" Core..=) Core.<$> mapping])

instance Core.FromJSON RecordColumn where
        parseJSON
          = Core.withObject "RecordColumn" Core.$
              \ x ->
                RecordColumn' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "SqlType" Core.<*>
                    x Core..:? "Mapping"
