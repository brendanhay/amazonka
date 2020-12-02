{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateCSVClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateCSVClassifierRequest where

import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a custom CSV classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createCSVClassifierRequest' smart constructor.
data CreateCSVClassifierRequest = CreateCSVClassifierRequest'
  { _cccrQuoteSymbol ::
      !(Maybe Text),
    _cccrContainsHeader ::
      !(Maybe CSVHeaderOption),
    _cccrDisableValueTrimming ::
      !(Maybe Bool),
    _cccrHeader :: !(Maybe [Text]),
    _cccrAllowSingleColumn ::
      !(Maybe Bool),
    _cccrDelimiter :: !(Maybe Text),
    _cccrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCSVClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccrQuoteSymbol' - A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
--
-- * 'cccrContainsHeader' - Indicates whether the CSV file contains a header.
--
-- * 'cccrDisableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is true.
--
-- * 'cccrHeader' - A list of strings representing column names.
--
-- * 'cccrAllowSingleColumn' - Enables the processing of files that contain only one column.
--
-- * 'cccrDelimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- * 'cccrName' - The name of the classifier.
createCSVClassifierRequest ::
  -- | 'cccrName'
  Text ->
  CreateCSVClassifierRequest
createCSVClassifierRequest pName_ =
  CreateCSVClassifierRequest'
    { _cccrQuoteSymbol = Nothing,
      _cccrContainsHeader = Nothing,
      _cccrDisableValueTrimming = Nothing,
      _cccrHeader = Nothing,
      _cccrAllowSingleColumn = Nothing,
      _cccrDelimiter = Nothing,
      _cccrName = pName_
    }

-- | A custom symbol to denote what combines content into a single column value. Must be different from the column delimiter.
cccrQuoteSymbol :: Lens' CreateCSVClassifierRequest (Maybe Text)
cccrQuoteSymbol = lens _cccrQuoteSymbol (\s a -> s {_cccrQuoteSymbol = a})

-- | Indicates whether the CSV file contains a header.
cccrContainsHeader :: Lens' CreateCSVClassifierRequest (Maybe CSVHeaderOption)
cccrContainsHeader = lens _cccrContainsHeader (\s a -> s {_cccrContainsHeader = a})

-- | Specifies not to trim values before identifying the type of column values. The default value is true.
cccrDisableValueTrimming :: Lens' CreateCSVClassifierRequest (Maybe Bool)
cccrDisableValueTrimming = lens _cccrDisableValueTrimming (\s a -> s {_cccrDisableValueTrimming = a})

-- | A list of strings representing column names.
cccrHeader :: Lens' CreateCSVClassifierRequest [Text]
cccrHeader = lens _cccrHeader (\s a -> s {_cccrHeader = a}) . _Default . _Coerce

-- | Enables the processing of files that contain only one column.
cccrAllowSingleColumn :: Lens' CreateCSVClassifierRequest (Maybe Bool)
cccrAllowSingleColumn = lens _cccrAllowSingleColumn (\s a -> s {_cccrAllowSingleColumn = a})

-- | A custom symbol to denote what separates each column entry in the row.
cccrDelimiter :: Lens' CreateCSVClassifierRequest (Maybe Text)
cccrDelimiter = lens _cccrDelimiter (\s a -> s {_cccrDelimiter = a})

-- | The name of the classifier.
cccrName :: Lens' CreateCSVClassifierRequest Text
cccrName = lens _cccrName (\s a -> s {_cccrName = a})

instance Hashable CreateCSVClassifierRequest

instance NFData CreateCSVClassifierRequest

instance ToJSON CreateCSVClassifierRequest where
  toJSON CreateCSVClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("QuoteSymbol" .=) <$> _cccrQuoteSymbol,
            ("ContainsHeader" .=) <$> _cccrContainsHeader,
            ("DisableValueTrimming" .=) <$> _cccrDisableValueTrimming,
            ("Header" .=) <$> _cccrHeader,
            ("AllowSingleColumn" .=) <$> _cccrAllowSingleColumn,
            ("Delimiter" .=) <$> _cccrDelimiter,
            Just ("Name" .= _cccrName)
          ]
      )
