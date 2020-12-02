{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GrokClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GrokClassifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A classifier that uses @grok@ patterns.
--
--
--
-- /See:/ 'grokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { _gcCreationTime ::
      !(Maybe POSIX),
    _gcLastUpdated :: !(Maybe POSIX),
    _gcVersion :: !(Maybe Integer),
    _gcCustomPatterns :: !(Maybe Text),
    _gcName :: !Text,
    _gcClassification :: !Text,
    _gcGrokPattern :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GrokClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCreationTime' - The time that this classifier was registered.
--
-- * 'gcLastUpdated' - The time that this classifier was last updated.
--
-- * 'gcVersion' - The version of this classifier.
--
-- * 'gcCustomPatterns' - Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
--
-- * 'gcName' - The name of the classifier.
--
-- * 'gcClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
--
-- * 'gcGrokPattern' - The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
grokClassifier ::
  -- | 'gcName'
  Text ->
  -- | 'gcClassification'
  Text ->
  -- | 'gcGrokPattern'
  Text ->
  GrokClassifier
grokClassifier pName_ pClassification_ pGrokPattern_ =
  GrokClassifier'
    { _gcCreationTime = Nothing,
      _gcLastUpdated = Nothing,
      _gcVersion = Nothing,
      _gcCustomPatterns = Nothing,
      _gcName = pName_,
      _gcClassification = pClassification_,
      _gcGrokPattern = pGrokPattern_
    }

-- | The time that this classifier was registered.
gcCreationTime :: Lens' GrokClassifier (Maybe UTCTime)
gcCreationTime = lens _gcCreationTime (\s a -> s {_gcCreationTime = a}) . mapping _Time

-- | The time that this classifier was last updated.
gcLastUpdated :: Lens' GrokClassifier (Maybe UTCTime)
gcLastUpdated = lens _gcLastUpdated (\s a -> s {_gcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
gcVersion :: Lens' GrokClassifier (Maybe Integer)
gcVersion = lens _gcVersion (\s a -> s {_gcVersion = a})

-- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
gcCustomPatterns :: Lens' GrokClassifier (Maybe Text)
gcCustomPatterns = lens _gcCustomPatterns (\s a -> s {_gcCustomPatterns = a})

-- | The name of the classifier.
gcName :: Lens' GrokClassifier Text
gcName = lens _gcName (\s a -> s {_gcName = a})

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
gcClassification :: Lens' GrokClassifier Text
gcClassification = lens _gcClassification (\s a -> s {_gcClassification = a})

-- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
gcGrokPattern :: Lens' GrokClassifier Text
gcGrokPattern = lens _gcGrokPattern (\s a -> s {_gcGrokPattern = a})

instance FromJSON GrokClassifier where
  parseJSON =
    withObject
      "GrokClassifier"
      ( \x ->
          GrokClassifier'
            <$> (x .:? "CreationTime")
            <*> (x .:? "LastUpdated")
            <*> (x .:? "Version")
            <*> (x .:? "CustomPatterns")
            <*> (x .: "Name")
            <*> (x .: "Classification")
            <*> (x .: "GrokPattern")
      )

instance Hashable GrokClassifier

instance NFData GrokClassifier
