{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CopyCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CopyCommand
  ( CopyCommand (..),

    -- * Smart constructor
    mkCopyCommand,

    -- * Lenses
    ccDataTableName,
    ccCopyOptions,
    ccDataTableColumns,
  )
where

import qualified Network.AWS.Firehose.Types.CopyOptions as Types
import qualified Network.AWS.Firehose.Types.DataTableColumns as Types
import qualified Network.AWS.Firehose.Types.DataTableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a @COPY@ command for Amazon Redshift.
--
-- /See:/ 'mkCopyCommand' smart constructor.
data CopyCommand = CopyCommand'
  { -- | The name of the target table. The table must already exist in the database.
    dataTableName :: Types.DataTableName,
    -- | Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows:
    --
    -- @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop.
    -- @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter).
    -- @delimiter '|' escape@ - the delimiter should be escaped.
    -- @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table.
    -- @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data.
    -- For more examples, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
    copyOptions :: Core.Maybe Types.CopyOptions,
    -- | A comma-separated list of column names.
    dataTableColumns :: Core.Maybe Types.DataTableColumns
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyCommand' value with any optional fields omitted.
mkCopyCommand ::
  -- | 'dataTableName'
  Types.DataTableName ->
  CopyCommand
mkCopyCommand dataTableName =
  CopyCommand'
    { dataTableName,
      copyOptions = Core.Nothing,
      dataTableColumns = Core.Nothing
    }

-- | The name of the target table. The table must already exist in the database.
--
-- /Note:/ Consider using 'dataTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDataTableName :: Lens.Lens' CopyCommand Types.DataTableName
ccDataTableName = Lens.field @"dataTableName"
{-# DEPRECATED ccDataTableName "Use generic-lens or generic-optics with 'dataTableName' instead." #-}

-- | Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows:
--
-- @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop.
-- @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter).
-- @delimiter '|' escape@ - the delimiter should be escaped.
-- @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table.
-- @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data.
-- For more examples, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
--
-- /Note:/ Consider using 'copyOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCopyOptions :: Lens.Lens' CopyCommand (Core.Maybe Types.CopyOptions)
ccCopyOptions = Lens.field @"copyOptions"
{-# DEPRECATED ccCopyOptions "Use generic-lens or generic-optics with 'copyOptions' instead." #-}

-- | A comma-separated list of column names.
--
-- /Note:/ Consider using 'dataTableColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDataTableColumns :: Lens.Lens' CopyCommand (Core.Maybe Types.DataTableColumns)
ccDataTableColumns = Lens.field @"dataTableColumns"
{-# DEPRECATED ccDataTableColumns "Use generic-lens or generic-optics with 'dataTableColumns' instead." #-}

instance Core.FromJSON CopyCommand where
  toJSON CopyCommand {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataTableName" Core..= dataTableName),
            ("CopyOptions" Core..=) Core.<$> copyOptions,
            ("DataTableColumns" Core..=) Core.<$> dataTableColumns
          ]
      )

instance Core.FromJSON CopyCommand where
  parseJSON =
    Core.withObject "CopyCommand" Core.$
      \x ->
        CopyCommand'
          Core.<$> (x Core..: "DataTableName")
          Core.<*> (x Core..:? "CopyOptions")
          Core.<*> (x Core..:? "DataTableColumns")
