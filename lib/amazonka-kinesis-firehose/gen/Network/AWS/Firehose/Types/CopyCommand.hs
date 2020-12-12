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
    ccCopyOptions,
    ccDataTableColumns,
    ccDataTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a @COPY@ command for Amazon Redshift.
--
-- /See:/ 'mkCopyCommand' smart constructor.
data CopyCommand = CopyCommand'
  { copyOptions ::
      Lude.Maybe Lude.Text,
    dataTableColumns :: Lude.Maybe Lude.Text,
    dataTableName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyCommand' with the minimum fields required to make a request.
--
-- * 'copyOptions' - Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows:
--
-- @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop.
-- @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter).
-- @delimiter '|' escape@ - the delimiter should be escaped.
-- @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table.
-- @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data.
-- For more examples, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
-- * 'dataTableColumns' - A comma-separated list of column names.
-- * 'dataTableName' - The name of the target table. The table must already exist in the database.
mkCopyCommand ::
  -- | 'dataTableName'
  Lude.Text ->
  CopyCommand
mkCopyCommand pDataTableName_ =
  CopyCommand'
    { copyOptions = Lude.Nothing,
      dataTableColumns = Lude.Nothing,
      dataTableName = pDataTableName_
    }

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
ccCopyOptions :: Lens.Lens' CopyCommand (Lude.Maybe Lude.Text)
ccCopyOptions = Lens.lens (copyOptions :: CopyCommand -> Lude.Maybe Lude.Text) (\s a -> s {copyOptions = a} :: CopyCommand)
{-# DEPRECATED ccCopyOptions "Use generic-lens or generic-optics with 'copyOptions' instead." #-}

-- | A comma-separated list of column names.
--
-- /Note:/ Consider using 'dataTableColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDataTableColumns :: Lens.Lens' CopyCommand (Lude.Maybe Lude.Text)
ccDataTableColumns = Lens.lens (dataTableColumns :: CopyCommand -> Lude.Maybe Lude.Text) (\s a -> s {dataTableColumns = a} :: CopyCommand)
{-# DEPRECATED ccDataTableColumns "Use generic-lens or generic-optics with 'dataTableColumns' instead." #-}

-- | The name of the target table. The table must already exist in the database.
--
-- /Note:/ Consider using 'dataTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDataTableName :: Lens.Lens' CopyCommand Lude.Text
ccDataTableName = Lens.lens (dataTableName :: CopyCommand -> Lude.Text) (\s a -> s {dataTableName = a} :: CopyCommand)
{-# DEPRECATED ccDataTableName "Use generic-lens or generic-optics with 'dataTableName' instead." #-}

instance Lude.FromJSON CopyCommand where
  parseJSON =
    Lude.withObject
      "CopyCommand"
      ( \x ->
          CopyCommand'
            Lude.<$> (x Lude..:? "CopyOptions")
            Lude.<*> (x Lude..:? "DataTableColumns")
            Lude.<*> (x Lude..: "DataTableName")
      )

instance Lude.ToJSON CopyCommand where
  toJSON CopyCommand' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CopyOptions" Lude..=) Lude.<$> copyOptions,
            ("DataTableColumns" Lude..=) Lude.<$> dataTableColumns,
            Lude.Just ("DataTableName" Lude..= dataTableName)
          ]
      )
