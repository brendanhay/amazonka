{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateGrokClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a grok classifier to update when passed to @UpdateClassifier@ .
--
--
--
-- /See:/ 'updateGrokClassifierRequest' smart constructor.
data UpdateGrokClassifierRequest = UpdateGrokClassifierRequest'
  { _ugcrClassification ::
      !(Maybe Text),
    _ugcrCustomPatterns ::
      !(Maybe Text),
    _ugcrGrokPattern :: !(Maybe Text),
    _ugcrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugcrClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- * 'ugcrCustomPatterns' - Optional custom grok patterns used by this classifier.
--
-- * 'ugcrGrokPattern' - The grok pattern used by this classifier.
--
-- * 'ugcrName' - The name of the @GrokClassifier@ .
updateGrokClassifierRequest ::
  -- | 'ugcrName'
  Text ->
  UpdateGrokClassifierRequest
updateGrokClassifierRequest pName_ =
  UpdateGrokClassifierRequest'
    { _ugcrClassification = Nothing,
      _ugcrCustomPatterns = Nothing,
      _ugcrGrokPattern = Nothing,
      _ugcrName = pName_
    }

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
ugcrClassification :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrClassification = lens _ugcrClassification (\s a -> s {_ugcrClassification = a})

-- | Optional custom grok patterns used by this classifier.
ugcrCustomPatterns :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrCustomPatterns = lens _ugcrCustomPatterns (\s a -> s {_ugcrCustomPatterns = a})

-- | The grok pattern used by this classifier.
ugcrGrokPattern :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrGrokPattern = lens _ugcrGrokPattern (\s a -> s {_ugcrGrokPattern = a})

-- | The name of the @GrokClassifier@ .
ugcrName :: Lens' UpdateGrokClassifierRequest Text
ugcrName = lens _ugcrName (\s a -> s {_ugcrName = a})

instance Hashable UpdateGrokClassifierRequest

instance NFData UpdateGrokClassifierRequest

instance ToJSON UpdateGrokClassifierRequest where
  toJSON UpdateGrokClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("Classification" .=) <$> _ugcrClassification,
            ("CustomPatterns" .=) <$> _ugcrCustomPatterns,
            ("GrokPattern" .=) <$> _ugcrGrokPattern,
            Just ("Name" .= _ugcrName)
          ]
      )
