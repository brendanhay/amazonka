{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CSVMappingParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the /'\n'/ as the row delimiter and a comma (",") as the column delimiter:
--
--
-- @"name1", "address1"@
--
-- @"name2", "address2"@
--
--
-- /See:/ 'csvMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { _cmpRecordRowDelimiter ::
      !Text,
    _cmpRecordColumnDelimiter :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CSVMappingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmpRecordRowDelimiter' - Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
--
-- * 'cmpRecordColumnDelimiter' - Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
csvMappingParameters ::
  -- | 'cmpRecordRowDelimiter'
  Text ->
  -- | 'cmpRecordColumnDelimiter'
  Text ->
  CSVMappingParameters
csvMappingParameters pRecordRowDelimiter_ pRecordColumnDelimiter_ =
  CSVMappingParameters'
    { _cmpRecordRowDelimiter =
        pRecordRowDelimiter_,
      _cmpRecordColumnDelimiter = pRecordColumnDelimiter_
    }

-- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
cmpRecordRowDelimiter :: Lens' CSVMappingParameters Text
cmpRecordRowDelimiter = lens _cmpRecordRowDelimiter (\s a -> s {_cmpRecordRowDelimiter = a})

-- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
cmpRecordColumnDelimiter :: Lens' CSVMappingParameters Text
cmpRecordColumnDelimiter = lens _cmpRecordColumnDelimiter (\s a -> s {_cmpRecordColumnDelimiter = a})

instance FromJSON CSVMappingParameters where
  parseJSON =
    withObject
      "CSVMappingParameters"
      ( \x ->
          CSVMappingParameters'
            <$> (x .: "RecordRowDelimiter") <*> (x .: "RecordColumnDelimiter")
      )

instance Hashable CSVMappingParameters

instance NFData CSVMappingParameters

instance ToJSON CSVMappingParameters where
  toJSON CSVMappingParameters' {..} =
    object
      ( catMaybes
          [ Just ("RecordRowDelimiter" .= _cmpRecordRowDelimiter),
            Just ("RecordColumnDelimiter" .= _cmpRecordColumnDelimiter)
          ]
      )
