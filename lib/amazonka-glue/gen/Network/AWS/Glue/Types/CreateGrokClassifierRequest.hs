{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateGrokClassifierRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { _cgcrCustomPatterns ::
      !(Maybe Text),
    _cgcrClassification :: !Text,
    _cgcrName :: !Text,
    _cgcrGrokPattern :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcrCustomPatterns' - Optional custom grok patterns used by this classifier.
--
-- * 'cgcrClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- * 'cgcrName' - The name of the new classifier.
--
-- * 'cgcrGrokPattern' - The grok pattern used by this classifier.
createGrokClassifierRequest ::
  -- | 'cgcrClassification'
  Text ->
  -- | 'cgcrName'
  Text ->
  -- | 'cgcrGrokPattern'
  Text ->
  CreateGrokClassifierRequest
createGrokClassifierRequest pClassification_ pName_ pGrokPattern_ =
  CreateGrokClassifierRequest'
    { _cgcrCustomPatterns = Nothing,
      _cgcrClassification = pClassification_,
      _cgcrName = pName_,
      _cgcrGrokPattern = pGrokPattern_
    }

-- | Optional custom grok patterns used by this classifier.
cgcrCustomPatterns :: Lens' CreateGrokClassifierRequest (Maybe Text)
cgcrCustomPatterns = lens _cgcrCustomPatterns (\s a -> s {_cgcrCustomPatterns = a})

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
cgcrClassification :: Lens' CreateGrokClassifierRequest Text
cgcrClassification = lens _cgcrClassification (\s a -> s {_cgcrClassification = a})

-- | The name of the new classifier.
cgcrName :: Lens' CreateGrokClassifierRequest Text
cgcrName = lens _cgcrName (\s a -> s {_cgcrName = a})

-- | The grok pattern used by this classifier.
cgcrGrokPattern :: Lens' CreateGrokClassifierRequest Text
cgcrGrokPattern = lens _cgcrGrokPattern (\s a -> s {_cgcrGrokPattern = a})

instance Hashable CreateGrokClassifierRequest

instance NFData CreateGrokClassifierRequest

instance ToJSON CreateGrokClassifierRequest where
  toJSON CreateGrokClassifierRequest' {..} =
    object
      ( catMaybes
          [ ("CustomPatterns" .=) <$> _cgcrCustomPatterns,
            Just ("Classification" .= _cgcrClassification),
            Just ("Name" .= _cgcrName),
            Just ("GrokPattern" .= _cgcrGrokPattern)
          ]
      )
