{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CSVInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CSVInput where

import Network.AWS.Glacier.Types.FileHeaderInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the comma-separated value (CSV) file to select from.
--
--
--
-- /See:/ 'csvInput' smart constructor.
data CSVInput = CSVInput'
  { _ciQuoteCharacter :: !(Maybe Text),
    _ciRecordDelimiter :: !(Maybe Text),
    _ciFileHeaderInfo :: !(Maybe FileHeaderInfo),
    _ciQuoteEscapeCharacter :: !(Maybe Text),
    _ciComments :: !(Maybe Text),
    _ciFieldDelimiter :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CSVInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciQuoteCharacter' - A value used as an escape character where the field delimiter is part of the value.
--
-- * 'ciRecordDelimiter' - A value used to separate individual records from each other.
--
-- * 'ciFileHeaderInfo' - Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
--
-- * 'ciQuoteEscapeCharacter' - A single character used for escaping the quotation-mark character inside an already escaped value.
--
-- * 'ciComments' - A single character used to indicate that a row should be ignored when the character is present at the start of that row.
--
-- * 'ciFieldDelimiter' - A value used to separate individual fields from each other within a record.
csvInput ::
  CSVInput
csvInput =
  CSVInput'
    { _ciQuoteCharacter = Nothing,
      _ciRecordDelimiter = Nothing,
      _ciFileHeaderInfo = Nothing,
      _ciQuoteEscapeCharacter = Nothing,
      _ciComments = Nothing,
      _ciFieldDelimiter = Nothing
    }

-- | A value used as an escape character where the field delimiter is part of the value.
ciQuoteCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteCharacter = lens _ciQuoteCharacter (\s a -> s {_ciQuoteCharacter = a})

-- | A value used to separate individual records from each other.
ciRecordDelimiter :: Lens' CSVInput (Maybe Text)
ciRecordDelimiter = lens _ciRecordDelimiter (\s a -> s {_ciRecordDelimiter = a})

-- | Describes the first line of input. Valid values are @None@ , @Ignore@ , and @Use@ .
ciFileHeaderInfo :: Lens' CSVInput (Maybe FileHeaderInfo)
ciFileHeaderInfo = lens _ciFileHeaderInfo (\s a -> s {_ciFileHeaderInfo = a})

-- | A single character used for escaping the quotation-mark character inside an already escaped value.
ciQuoteEscapeCharacter :: Lens' CSVInput (Maybe Text)
ciQuoteEscapeCharacter = lens _ciQuoteEscapeCharacter (\s a -> s {_ciQuoteEscapeCharacter = a})

-- | A single character used to indicate that a row should be ignored when the character is present at the start of that row.
ciComments :: Lens' CSVInput (Maybe Text)
ciComments = lens _ciComments (\s a -> s {_ciComments = a})

-- | A value used to separate individual fields from each other within a record.
ciFieldDelimiter :: Lens' CSVInput (Maybe Text)
ciFieldDelimiter = lens _ciFieldDelimiter (\s a -> s {_ciFieldDelimiter = a})

instance FromJSON CSVInput where
  parseJSON =
    withObject
      "CSVInput"
      ( \x ->
          CSVInput'
            <$> (x .:? "QuoteCharacter")
            <*> (x .:? "RecordDelimiter")
            <*> (x .:? "FileHeaderInfo")
            <*> (x .:? "QuoteEscapeCharacter")
            <*> (x .:? "Comments")
            <*> (x .:? "FieldDelimiter")
      )

instance Hashable CSVInput

instance NFData CSVInput

instance ToJSON CSVInput where
  toJSON CSVInput' {..} =
    object
      ( catMaybes
          [ ("QuoteCharacter" .=) <$> _ciQuoteCharacter,
            ("RecordDelimiter" .=) <$> _ciRecordDelimiter,
            ("FileHeaderInfo" .=) <$> _ciFileHeaderInfo,
            ("QuoteEscapeCharacter" .=) <$> _ciQuoteEscapeCharacter,
            ("Comments" .=) <$> _ciComments,
            ("FieldDelimiter" .=) <$> _ciFieldDelimiter
          ]
      )
