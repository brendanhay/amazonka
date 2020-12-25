{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
  ( CSVMappingParameters (..),

    -- * Smart constructor
    mkCSVMappingParameters,

    -- * Lenses
    csvmpRecordRowDelimiter,
    csvmpRecordColumnDelimiter,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.RecordColumnDelimiter as Types
import qualified Network.AWS.KinesisAnalytics.Types.RecordRowDelimiter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the /'\n'/ as the row delimiter and a comma (",") as the column delimiter:
--
-- @"name1", "address1"@
-- @"name2", "address2"@
--
-- /See:/ 'mkCSVMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { -- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
    recordRowDelimiter :: Types.RecordRowDelimiter,
    -- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
    recordColumnDelimiter :: Types.RecordColumnDelimiter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CSVMappingParameters' value with any optional fields omitted.
mkCSVMappingParameters ::
  -- | 'recordRowDelimiter'
  Types.RecordRowDelimiter ->
  -- | 'recordColumnDelimiter'
  Types.RecordColumnDelimiter ->
  CSVMappingParameters
mkCSVMappingParameters recordRowDelimiter recordColumnDelimiter =
  CSVMappingParameters' {recordRowDelimiter, recordColumnDelimiter}

-- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
--
-- /Note:/ Consider using 'recordRowDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvmpRecordRowDelimiter :: Lens.Lens' CSVMappingParameters Types.RecordRowDelimiter
csvmpRecordRowDelimiter = Lens.field @"recordRowDelimiter"
{-# DEPRECATED csvmpRecordRowDelimiter "Use generic-lens or generic-optics with 'recordRowDelimiter' instead." #-}

-- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
--
-- /Note:/ Consider using 'recordColumnDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csvmpRecordColumnDelimiter :: Lens.Lens' CSVMappingParameters Types.RecordColumnDelimiter
csvmpRecordColumnDelimiter = Lens.field @"recordColumnDelimiter"
{-# DEPRECATED csvmpRecordColumnDelimiter "Use generic-lens or generic-optics with 'recordColumnDelimiter' instead." #-}

instance Core.FromJSON CSVMappingParameters where
  toJSON CSVMappingParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RecordRowDelimiter" Core..= recordRowDelimiter),
            Core.Just ("RecordColumnDelimiter" Core..= recordColumnDelimiter)
          ]
      )

instance Core.FromJSON CSVMappingParameters where
  parseJSON =
    Core.withObject "CSVMappingParameters" Core.$
      \x ->
        CSVMappingParameters'
          Core.<$> (x Core..: "RecordRowDelimiter")
          Core.<*> (x Core..: "RecordColumnDelimiter")
