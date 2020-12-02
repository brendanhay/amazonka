{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CopyCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CopyCommand where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a @COPY@ command for Amazon Redshift.
--
--
--
-- /See:/ 'copyCommand' smart constructor.
data CopyCommand = CopyCommand'
  { _ccCopyOptions :: !(Maybe Text),
    _ccDataTableColumns :: !(Maybe Text),
    _ccDataTableName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCopyOptions' - Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows: @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
--
-- * 'ccDataTableColumns' - A comma-separated list of column names.
--
-- * 'ccDataTableName' - The name of the target table. The table must already exist in the database.
copyCommand ::
  -- | 'ccDataTableName'
  Text ->
  CopyCommand
copyCommand pDataTableName_ =
  CopyCommand'
    { _ccCopyOptions = Nothing,
      _ccDataTableColumns = Nothing,
      _ccDataTableName = pDataTableName_
    }

-- | Optional parameters to use with the Amazon Redshift @COPY@ command. For more information, see the "Optional Parameters" section of <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift COPY command> . Some possible examples that would apply to Kinesis Data Firehose are as follows: @delimiter '\t' lzop;@ - fields are delimited with "\t" (TAB character) and compressed using lzop. @delimiter '|'@ - fields are delimited with "|" (this is the default delimiter). @delimiter '|' escape@ - the delimiter should be escaped. @fixedwidth 'venueid:3,venuename:25,venuecity:12,venuestate:2,venueseats:6'@ - fields are fixed width in the source, with each width specified after every column in the table. @JSON 's3://mybucket/jsonpaths.txt'@ - data is in JSON format, and the path specified is the format of the data. For more examples, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY_command_examples.html Amazon Redshift COPY command examples> .
ccCopyOptions :: Lens' CopyCommand (Maybe Text)
ccCopyOptions = lens _ccCopyOptions (\s a -> s {_ccCopyOptions = a})

-- | A comma-separated list of column names.
ccDataTableColumns :: Lens' CopyCommand (Maybe Text)
ccDataTableColumns = lens _ccDataTableColumns (\s a -> s {_ccDataTableColumns = a})

-- | The name of the target table. The table must already exist in the database.
ccDataTableName :: Lens' CopyCommand Text
ccDataTableName = lens _ccDataTableName (\s a -> s {_ccDataTableName = a})

instance FromJSON CopyCommand where
  parseJSON =
    withObject
      "CopyCommand"
      ( \x ->
          CopyCommand'
            <$> (x .:? "CopyOptions")
            <*> (x .:? "DataTableColumns")
            <*> (x .: "DataTableName")
      )

instance Hashable CopyCommand

instance NFData CopyCommand

instance ToJSON CopyCommand where
  toJSON CopyCommand' {..} =
    object
      ( catMaybes
          [ ("CopyOptions" .=) <$> _ccCopyOptions,
            ("DataTableColumns" .=) <$> _ccDataTableColumns,
            Just ("DataTableName" .= _ccDataTableName)
          ]
      )
