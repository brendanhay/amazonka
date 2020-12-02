{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CSVOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CSVOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.QuoteFields

-- | Describes how uncompressed comma-separated values (CSV)-formatted results are formatted.
--
--
--
-- /See:/ 'csvOutput' smart constructor.
data CSVOutput = CSVOutput'
  { _coQuoteCharacter :: !(Maybe Text),
    _coQuoteFields :: !(Maybe QuoteFields),
    _coRecordDelimiter :: !(Maybe Text),
    _coQuoteEscapeCharacter :: !(Maybe Text),
    _coFieldDelimiter :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CSVOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coQuoteCharacter' - A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
--
-- * 'coQuoteFields' - Indicates whether to use quotation marks around output fields.      * @ALWAYS@ : Always use quotation marks for output fields.     * @ASNEEDED@ : Use quotation marks for output fields when needed.
--
-- * 'coRecordDelimiter' - A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
--
-- * 'coQuoteEscapeCharacter' - The single character used for escaping the quote character inside an already escaped value.
--
-- * 'coFieldDelimiter' - The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
csvOutput ::
  CSVOutput
csvOutput =
  CSVOutput'
    { _coQuoteCharacter = Nothing,
      _coQuoteFields = Nothing,
      _coRecordDelimiter = Nothing,
      _coQuoteEscapeCharacter = Nothing,
      _coFieldDelimiter = Nothing
    }

-- | A single character used for escaping when the field delimiter is part of the value. For example, if the value is @a, b@ , Amazon S3 wraps this field value in quotation marks, as follows: @" a , b "@ .
coQuoteCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteCharacter = lens _coQuoteCharacter (\s a -> s {_coQuoteCharacter = a})

-- | Indicates whether to use quotation marks around output fields.      * @ALWAYS@ : Always use quotation marks for output fields.     * @ASNEEDED@ : Use quotation marks for output fields when needed.
coQuoteFields :: Lens' CSVOutput (Maybe QuoteFields)
coQuoteFields = lens _coQuoteFields (\s a -> s {_coQuoteFields = a})

-- | A single character used to separate individual records in the output. Instead of the default value, you can specify an arbitrary delimiter.
coRecordDelimiter :: Lens' CSVOutput (Maybe Text)
coRecordDelimiter = lens _coRecordDelimiter (\s a -> s {_coRecordDelimiter = a})

-- | The single character used for escaping the quote character inside an already escaped value.
coQuoteEscapeCharacter :: Lens' CSVOutput (Maybe Text)
coQuoteEscapeCharacter = lens _coQuoteEscapeCharacter (\s a -> s {_coQuoteEscapeCharacter = a})

-- | The value used to separate individual fields in a record. You can specify an arbitrary delimiter.
coFieldDelimiter :: Lens' CSVOutput (Maybe Text)
coFieldDelimiter = lens _coFieldDelimiter (\s a -> s {_coFieldDelimiter = a})

instance Hashable CSVOutput

instance NFData CSVOutput

instance ToXML CSVOutput where
  toXML CSVOutput' {..} =
    mconcat
      [ "QuoteCharacter" @= _coQuoteCharacter,
        "QuoteFields" @= _coQuoteFields,
        "RecordDelimiter" @= _coRecordDelimiter,
        "QuoteEscapeCharacter" @= _coQuoteEscapeCharacter,
        "FieldDelimiter" @= _coFieldDelimiter
      ]
