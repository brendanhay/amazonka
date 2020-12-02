{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateCSVClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateCSVClassifierRequest where

import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a custom CSV classifier to be updated.
--
--
--
-- /See:/ 'updateCSVClassifierRequest' smart constructor.
data UpdateCSVClassifierRequest = UpdateCSVClassifierRequest'
  { _uccrQuoteSymbol ::
      !(Maybe Text),
    _uccrContainsHeader ::
      !(Maybe CSVHeaderOption),
    _uccrDisableValueTrimming ::
      !(Maybe Bool),
    _uccrHeader :: !(Maybe [Text]),
    _uccrAllowSingleColumn ::
      !(Maybe Bool),
    _uccrDelimiter :: !(Maybe Text),
    _uccrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateCSVClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccrQuoteSymbol' - A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- * 'uccrContainsHeader' - Indicates whether the CSV file contains a header.
--
-- * 'uccrDisableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is true.
--
-- * 'uccrHeader' - A list of strings representing column names.
--
-- * 'uccrAllowSingleColumn' - Enables the processing of files that contain only one column.
--
-- * 'uccrDelimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- * 'uccrName' - The name of the classifier.
updateCSVClassifierRequest ::
  -- | 'uccrName'
  Text ->
  UpdateCSVClassifierRequest
updateCSVClassifierRequest pName_ =
  UpdateCSVClassifierRequest'
    { _uccrQuoteSymbol = Nothing,
      _uccrContainsHeader = Nothing,
      _uccrDisableValueTrimming = Nothing,
      _uccrHeader = Nothing,
      _uccrAllowSingleColumn = Nothing,
      _uccrDelimiter = Nothing,
      _uccrName = pName_
    }

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
uccrQuoteSymbol :: Lens' UpdateCSVClassifierRequest (Maybe Text)
uccrQuoteSymbol = lens _uccrQuoteSymbol (\s a -> s {_uccrQuoteSymbol = a})

-- | Indicates whether the CSV file contains a header.
uccrContainsHeader :: Lens' UpdateCSVClassifierRequest (Maybe CSVHeaderOption)
uccrContainsHeader = lens _uccrContainsHeader (\s a -> s {_uccrContainsHeader = a})

-- | Specifies not to trim values before identifying the type of column values. The default value is true.
uccrDisableValueTrimming :: Lens' UpdateCSVClassifierRequest (Maybe Bool)
uccrDisableValueTrimming = lens _uccrDisableValueTrimming (\s a -> s {_uccrDisableValueTrimming = a})

-- | A list of strings representing column names.
uccrHeader :: Lens' UpdateCSVClassifierRequest [Text]
uccrHeader = lens _uccrHeader (\s a -> s {_uccrHeader = a}) . _Default . _Coerce

-- | Enables the processing of files that contain only one column.
uccrAllowSingleColumn :: Lens' UpdateCSVClassifierRequest (Maybe Bool)
uccrAllowSingleColumn = lens _uccrAllowSingleColumn (\s a -> s {_uccrAllowSingleColumn = a})

-- | A custom symbol to denote what separates each column entry in the row.
uccrDelimiter :: Lens' UpdateCSVClassifierRequest (Maybe Text)
uccrDelimiter = lens _uccrDelimiter (\s a -> s {_uccrDelimiter = a})

-- | The name of the classifier.
uccrName :: Lens' UpdateCSVClassifierRequest Text
uccrName = lens _uccrName (\s a -> s {_uccrName = a})

instance Hashable UpdateCSVClassifierRequest

instance NFData UpdateCSVClassifierRequest

instance ToJSON UpdateCSVClassifierRequest where
  toJSON UpdateCSVClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("QuoteSymbol" .=) <$> _uccrQuoteSymbol,
            ("ContainsHeader" .=) <$> _uccrContainsHeader,
            ("DisableValueTrimming" .=) <$> _uccrDisableValueTrimming,
            ("Header" .=) <$> _uccrHeader,
            ("AllowSingleColumn" .=) <$> _uccrAllowSingleColumn,
            ("Delimiter" .=) <$> _uccrDelimiter,
            Just ("Name" .= _uccrName)
          ]
      )
