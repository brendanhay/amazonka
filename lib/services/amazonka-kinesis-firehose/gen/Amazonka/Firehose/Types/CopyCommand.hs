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
-- Module      : Amazonka.Firehose.Types.CopyCommand
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.CopyCommand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    copyOptions :: Prelude.Maybe Prelude.Text,
    -- | A comma-separated list of column names.
    dataTableColumns :: Prelude.Maybe Prelude.Text,
    -- | The name of the target table. The table must already exist in the
    -- database.
    dataTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CopyCommand
newCopyCommand pDataTableName_ =
  CopyCommand'
    { copyOptions = Prelude.Nothing,
      dataTableColumns = Prelude.Nothing,
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
copyCommand_copyOptions :: Lens.Lens' CopyCommand (Prelude.Maybe Prelude.Text)
copyCommand_copyOptions = Lens.lens (\CopyCommand' {copyOptions} -> copyOptions) (\s@CopyCommand' {} a -> s {copyOptions = a} :: CopyCommand)

-- | A comma-separated list of column names.
copyCommand_dataTableColumns :: Lens.Lens' CopyCommand (Prelude.Maybe Prelude.Text)
copyCommand_dataTableColumns = Lens.lens (\CopyCommand' {dataTableColumns} -> dataTableColumns) (\s@CopyCommand' {} a -> s {dataTableColumns = a} :: CopyCommand)

-- | The name of the target table. The table must already exist in the
-- database.
copyCommand_dataTableName :: Lens.Lens' CopyCommand Prelude.Text
copyCommand_dataTableName = Lens.lens (\CopyCommand' {dataTableName} -> dataTableName) (\s@CopyCommand' {} a -> s {dataTableName = a} :: CopyCommand)

instance Data.FromJSON CopyCommand where
  parseJSON =
    Data.withObject
      "CopyCommand"
      ( \x ->
          CopyCommand'
            Prelude.<$> (x Data..:? "CopyOptions")
            Prelude.<*> (x Data..:? "DataTableColumns")
            Prelude.<*> (x Data..: "DataTableName")
      )

instance Prelude.Hashable CopyCommand where
  hashWithSalt _salt CopyCommand' {..} =
    _salt `Prelude.hashWithSalt` copyOptions
      `Prelude.hashWithSalt` dataTableColumns
      `Prelude.hashWithSalt` dataTableName

instance Prelude.NFData CopyCommand where
  rnf CopyCommand' {..} =
    Prelude.rnf copyOptions
      `Prelude.seq` Prelude.rnf dataTableColumns
      `Prelude.seq` Prelude.rnf dataTableName

instance Data.ToJSON CopyCommand where
  toJSON CopyCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CopyOptions" Data..=) Prelude.<$> copyOptions,
            ("DataTableColumns" Data..=)
              Prelude.<$> dataTableColumns,
            Prelude.Just
              ("DataTableName" Data..= dataTableName)
          ]
      )
