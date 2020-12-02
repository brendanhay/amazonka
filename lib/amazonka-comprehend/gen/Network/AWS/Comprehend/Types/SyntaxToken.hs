{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SyntaxToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxToken where

import Network.AWS.Comprehend.Types.PartOfSpeechTag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a work in the input text that was recognized and assigned a part of speech. There is one syntax token record for each word in the source text.
--
--
--
-- /See:/ 'syntaxToken' smart constructor.
data SyntaxToken = SyntaxToken'
  { _stBeginOffset :: !(Maybe Int),
    _stText :: !(Maybe Text),
    _stTokenId :: !(Maybe Int),
    _stEndOffset :: !(Maybe Int),
    _stPartOfSpeech :: !(Maybe PartOfSpeechTag)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SyntaxToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stBeginOffset' - The zero-based offset from the beginning of the source text to the first character in the word.
--
-- * 'stText' - The word that was recognized in the source text.
--
-- * 'stTokenId' - A unique identifier for a token.
--
-- * 'stEndOffset' - The zero-based offset from the beginning of the source text to the last character in the word.
--
-- * 'stPartOfSpeech' - Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
syntaxToken ::
  SyntaxToken
syntaxToken =
  SyntaxToken'
    { _stBeginOffset = Nothing,
      _stText = Nothing,
      _stTokenId = Nothing,
      _stEndOffset = Nothing,
      _stPartOfSpeech = Nothing
    }

-- | The zero-based offset from the beginning of the source text to the first character in the word.
stBeginOffset :: Lens' SyntaxToken (Maybe Int)
stBeginOffset = lens _stBeginOffset (\s a -> s {_stBeginOffset = a})

-- | The word that was recognized in the source text.
stText :: Lens' SyntaxToken (Maybe Text)
stText = lens _stText (\s a -> s {_stText = a})

-- | A unique identifier for a token.
stTokenId :: Lens' SyntaxToken (Maybe Int)
stTokenId = lens _stTokenId (\s a -> s {_stTokenId = a})

-- | The zero-based offset from the beginning of the source text to the last character in the word.
stEndOffset :: Lens' SyntaxToken (Maybe Int)
stEndOffset = lens _stEndOffset (\s a -> s {_stEndOffset = a})

-- | Provides the part of speech label and the confidence level that Amazon Comprehend has that the part of speech was correctly identified. For more information, see 'how-syntax' .
stPartOfSpeech :: Lens' SyntaxToken (Maybe PartOfSpeechTag)
stPartOfSpeech = lens _stPartOfSpeech (\s a -> s {_stPartOfSpeech = a})

instance FromJSON SyntaxToken where
  parseJSON =
    withObject
      "SyntaxToken"
      ( \x ->
          SyntaxToken'
            <$> (x .:? "BeginOffset")
            <*> (x .:? "Text")
            <*> (x .:? "TokenId")
            <*> (x .:? "EndOffset")
            <*> (x .:? "PartOfSpeech")
      )

instance Hashable SyntaxToken

instance NFData SyntaxToken
