{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CopyCommand
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CopyCommand where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a @COPY@ command for Amazon Redshift.
--
-- /See:/ 'newCopyCommand' smart constructor.
data CopyCommand = CopyCommand'
  { -- | Optional parameters to use with the Amazon Redshift @COPY@ command. For
    -- more information, see the \"Optional Parameters\" section of
    -- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command>.
    -- Some possible examples that would apply to Kinesis Data Firehose are as
    -- follows:
    --
    -- @delimiter \'\\t\' lzop;@ - fields are delimited with \"\\t\" (TAB
    -- character) and compressed using lzop.
    --
    -- @delimiter \'|\'@ - fields are delimited with \"|\" (this is the default
    -- delimiter).
    --
    -- @delimiter \'|\' escape@ - the delimiter should be escaped.
    --
    -- @fixedwidth \'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6\'@
    -- - fields are fixed width in the source, with each width specified after
    -- every column in the table.
    --
    -- @JSON \'s3:\/\/mybucket\/jsonpaths.txt\'@ - data is in JSON format, and
    -- the path specified is the format of the data.
    --
    -- For more examples, see
    -- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples>.
    copyOptions :: Core.Maybe Core.Text,
    -- | A comma-separated list of column names.
    dataTableColumns :: Core.Maybe Core.Text,
    -- | The name of the target table. The table must already exist in the
    -- database.
    dataTableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopyCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyOptions', 'copyCommand_copyOptions' - Optional parameters to use with the Amazon Redshift @COPY@ command. For
-- more information, see the \"Optional Parameters\" section of
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command>.
-- Some possible examples that would apply to Kinesis Data Firehose are as
-- follows:
--
-- @delimiter \'\\t\' lzop;@ - fields are delimited with \"\\t\" (TAB
-- character) and compressed using lzop.
--
-- @delimiter \'|\'@ - fields are delimited with \"|\" (this is the default
-- delimiter).
--
-- @delimiter \'|\' escape@ - the delimiter should be escaped.
--
-- @fixedwidth \'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6\'@
-- - fields are fixed width in the source, with each width specified after
-- every column in the table.
--
-- @JSON \'s3:\/\/mybucket\/jsonpaths.txt\'@ - data is in JSON format, and
-- the path specified is the format of the data.
--
-- For more examples, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples>.
--
-- 'dataTableColumns', 'copyCommand_dataTableColumns' - A comma-separated list of column names.
--
-- 'dataTableName', 'copyCommand_dataTableName' - The name of the target table. The table must already exist in the
-- database.
newCopyCommand ::
  -- | 'dataTableName'
  Core.Text ->
  CopyCommand
newCopyCommand pDataTableName_ =
  CopyCommand'
    { copyOptions = Core.Nothing,
      dataTableColumns = Core.Nothing,
      dataTableName = pDataTableName_
    }

-- | Optional parameters to use with the Amazon Redshift @COPY@ command. For
-- more information, see the \"Optional Parameters\" section of
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command>.
-- Some possible examples that would apply to Kinesis Data Firehose are as
-- follows:
--
-- @delimiter \'\\t\' lzop;@ - fields are delimited with \"\\t\" (TAB
-- character) and compressed using lzop.
--
-- @delimiter \'|\'@ - fields are delimited with \"|\" (this is the default
-- delimiter).
--
-- @delimiter \'|\' escape@ - the delimiter should be escaped.
--
-- @fixedwidth \'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6\'@
-- - fields are fixed width in the source, with each width specified after
-- every column in the table.
--
-- @JSON \'s3:\/\/mybucket\/jsonpaths.txt\'@ - data is in JSON format, and
-- the path specified is the format of the data.
--
-- For more examples, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples>.
copyCommand_copyOptions :: Lens.Lens' CopyCommand (Core.Maybe Core.Text)
copyCommand_copyOptions = Lens.lens (\CopyCommand' {copyOptions} -> copyOptions) (\s@CopyCommand' {} a -> s {copyOptions = a} :: CopyCommand)

-- | A comma-separated list of column names.
copyCommand_dataTableColumns :: Lens.Lens' CopyCommand (Core.Maybe Core.Text)
copyCommand_dataTableColumns = Lens.lens (\CopyCommand' {dataTableColumns} -> dataTableColumns) (\s@CopyCommand' {} a -> s {dataTableColumns = a} :: CopyCommand)

-- | The name of the target table. The table must already exist in the
-- database.
copyCommand_dataTableName :: Lens.Lens' CopyCommand Core.Text
copyCommand_dataTableName = Lens.lens (\CopyCommand' {dataTableName} -> dataTableName) (\s@CopyCommand' {} a -> s {dataTableName = a} :: CopyCommand)

instance Core.FromJSON CopyCommand where
  parseJSON =
    Core.withObject
      "CopyCommand"
      ( \x ->
          CopyCommand'
            Core.<$> (x Core..:? "CopyOptions")
            Core.<*> (x Core..:? "DataTableColumns")
            Core.<*> (x Core..: "DataTableName")
      )

instance Core.Hashable CopyCommand

instance Core.NFData CopyCommand

instance Core.ToJSON CopyCommand where
  toJSON CopyCommand' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CopyOptions" Core..=) Core.<$> copyOptions,
            ("DataTableColumns" Core..=)
              Core.<$> dataTableColumns,
            Core.Just ("DataTableName" Core..= dataTableName)
          ]
      )
