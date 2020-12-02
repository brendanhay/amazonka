{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Classifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Classifier where

import Network.AWS.Glue.Types.CSVClassifier
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.JSONClassifier
import Network.AWS.Glue.Types.XMLClassifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Classifiers are triggered during a crawl task. A classifier checks whether a given file is in a format it can handle. If it is, the classifier creates a schema in the form of a @StructType@ object that matches that data format.
--
--
-- You can use the standard classifiers that AWS Glue provides, or you can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier can be a @grok@ classifier, an @XML@ classifier, a @JSON@ classifier, or a custom @CSV@ classifier, as specified in one of the fields in the @Classifier@ object.
--
--
-- /See:/ 'classifier' smart constructor.
data Classifier = Classifier'
  { _cGrokClassifier ::
      !(Maybe GrokClassifier),
    _cXMLClassifier :: !(Maybe XMLClassifier),
    _cCSVClassifier :: !(Maybe CSVClassifier),
    _cJSONClassifier :: !(Maybe JSONClassifier)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Classifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cGrokClassifier' - A classifier that uses @grok@ .
--
-- * 'cXMLClassifier' - A classifier for XML content.
--
-- * 'cCSVClassifier' - A classifier for comma-separated values (CSV).
--
-- * 'cJSONClassifier' - A classifier for JSON content.
classifier ::
  Classifier
classifier =
  Classifier'
    { _cGrokClassifier = Nothing,
      _cXMLClassifier = Nothing,
      _cCSVClassifier = Nothing,
      _cJSONClassifier = Nothing
    }

-- | A classifier that uses @grok@ .
cGrokClassifier :: Lens' Classifier (Maybe GrokClassifier)
cGrokClassifier = lens _cGrokClassifier (\s a -> s {_cGrokClassifier = a})

-- | A classifier for XML content.
cXMLClassifier :: Lens' Classifier (Maybe XMLClassifier)
cXMLClassifier = lens _cXMLClassifier (\s a -> s {_cXMLClassifier = a})

-- | A classifier for comma-separated values (CSV).
cCSVClassifier :: Lens' Classifier (Maybe CSVClassifier)
cCSVClassifier = lens _cCSVClassifier (\s a -> s {_cCSVClassifier = a})

-- | A classifier for JSON content.
cJSONClassifier :: Lens' Classifier (Maybe JSONClassifier)
cJSONClassifier = lens _cJSONClassifier (\s a -> s {_cJSONClassifier = a})

instance FromJSON Classifier where
  parseJSON =
    withObject
      "Classifier"
      ( \x ->
          Classifier'
            <$> (x .:? "GrokClassifier")
            <*> (x .:? "XMLClassifier")
            <*> (x .:? "CsvClassifier")
            <*> (x .:? "JsonClassifier")
      )

instance Hashable Classifier

instance NFData Classifier
