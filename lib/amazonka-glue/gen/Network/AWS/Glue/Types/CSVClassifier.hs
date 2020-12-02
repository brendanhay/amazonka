{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CSVClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CSVClassifier where

import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A classifier for custom @CSV@ content.
--
--
--
-- /See:/ 'csvClassifier' smart constructor.
data CSVClassifier = CSVClassifier'
  { _csvcCreationTime ::
      !(Maybe POSIX),
    _csvcQuoteSymbol :: !(Maybe Text),
    _csvcContainsHeader :: !(Maybe CSVHeaderOption),
    _csvcLastUpdated :: !(Maybe POSIX),
    _csvcDisableValueTrimming :: !(Maybe Bool),
    _csvcHeader :: !(Maybe [Text]),
    _csvcVersion :: !(Maybe Integer),
    _csvcAllowSingleColumn :: !(Maybe Bool),
    _csvcDelimiter :: !(Maybe Text),
    _csvcName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CSVClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csvcCreationTime' - The time that this classifier was registered.
--
-- * 'csvcQuoteSymbol' - A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
--
-- * 'csvcContainsHeader' - Indicates whether the CSV file contains a header.
--
-- * 'csvcLastUpdated' - The time that this classifier was last updated.
--
-- * 'csvcDisableValueTrimming' - Specifies not to trim values before identifying the type of column values. The default value is @true@ .
--
-- * 'csvcHeader' - A list of strings representing column names.
--
-- * 'csvcVersion' - The version of this classifier.
--
-- * 'csvcAllowSingleColumn' - Enables the processing of files that contain only one column.
--
-- * 'csvcDelimiter' - A custom symbol to denote what separates each column entry in the row.
--
-- * 'csvcName' - The name of the classifier.
csvClassifier ::
  -- | 'csvcName'
  Text ->
  CSVClassifier
csvClassifier pName_ =
  CSVClassifier'
    { _csvcCreationTime = Nothing,
      _csvcQuoteSymbol = Nothing,
      _csvcContainsHeader = Nothing,
      _csvcLastUpdated = Nothing,
      _csvcDisableValueTrimming = Nothing,
      _csvcHeader = Nothing,
      _csvcVersion = Nothing,
      _csvcAllowSingleColumn = Nothing,
      _csvcDelimiter = Nothing,
      _csvcName = pName_
    }

-- | The time that this classifier was registered.
csvcCreationTime :: Lens' CSVClassifier (Maybe UTCTime)
csvcCreationTime = lens _csvcCreationTime (\s a -> s {_csvcCreationTime = a}) . mapping _Time

-- | A custom symbol to denote what combines content into a single column value. It must be different from the column delimiter.
csvcQuoteSymbol :: Lens' CSVClassifier (Maybe Text)
csvcQuoteSymbol = lens _csvcQuoteSymbol (\s a -> s {_csvcQuoteSymbol = a})

-- | Indicates whether the CSV file contains a header.
csvcContainsHeader :: Lens' CSVClassifier (Maybe CSVHeaderOption)
csvcContainsHeader = lens _csvcContainsHeader (\s a -> s {_csvcContainsHeader = a})

-- | The time that this classifier was last updated.
csvcLastUpdated :: Lens' CSVClassifier (Maybe UTCTime)
csvcLastUpdated = lens _csvcLastUpdated (\s a -> s {_csvcLastUpdated = a}) . mapping _Time

-- | Specifies not to trim values before identifying the type of column values. The default value is @true@ .
csvcDisableValueTrimming :: Lens' CSVClassifier (Maybe Bool)
csvcDisableValueTrimming = lens _csvcDisableValueTrimming (\s a -> s {_csvcDisableValueTrimming = a})

-- | A list of strings representing column names.
csvcHeader :: Lens' CSVClassifier [Text]
csvcHeader = lens _csvcHeader (\s a -> s {_csvcHeader = a}) . _Default . _Coerce

-- | The version of this classifier.
csvcVersion :: Lens' CSVClassifier (Maybe Integer)
csvcVersion = lens _csvcVersion (\s a -> s {_csvcVersion = a})

-- | Enables the processing of files that contain only one column.
csvcAllowSingleColumn :: Lens' CSVClassifier (Maybe Bool)
csvcAllowSingleColumn = lens _csvcAllowSingleColumn (\s a -> s {_csvcAllowSingleColumn = a})

-- | A custom symbol to denote what separates each column entry in the row.
csvcDelimiter :: Lens' CSVClassifier (Maybe Text)
csvcDelimiter = lens _csvcDelimiter (\s a -> s {_csvcDelimiter = a})

-- | The name of the classifier.
csvcName :: Lens' CSVClassifier Text
csvcName = lens _csvcName (\s a -> s {_csvcName = a})

instance FromJSON CSVClassifier where
  parseJSON =
    withObject
      "CSVClassifier"
      ( \x ->
          CSVClassifier'
            <$> (x .:? "CreationTime")
            <*> (x .:? "QuoteSymbol")
            <*> (x .:? "ContainsHeader")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "DisableValueTrimming")
            <*> (x .:? "Header" .!= mempty)
            <*> (x .:? "Version")
            <*> (x .:? "AllowSingleColumn")
            <*> (x .:? "Delimiter")
            <*> (x .: "Name")
      )

instance Hashable CSVClassifier

instance NFData CSVClassifier
