{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Prompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Prompt where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Message
import Network.AWS.Prelude

-- | Obtains information from the user. To define a prompt, provide one or more messages and specify the number of attempts to get information from the user. If you provide more than one message, Amazon Lex chooses one of the messages to use to prompt the user. For more information, see 'how-it-works' .
--
--
--
-- /See:/ 'prompt' smart constructor.
data Prompt = Prompt'
  { _pResponseCard :: !(Maybe Text),
    _pMessages :: !(List1 Message),
    _pMaxAttempts :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Prompt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pResponseCard' - A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
--
-- * 'pMessages' - An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
--
-- * 'pMaxAttempts' - The number of times to prompt the user for information.
prompt ::
  -- | 'pMessages'
  NonEmpty Message ->
  -- | 'pMaxAttempts'
  Natural ->
  Prompt
prompt pMessages_ pMaxAttempts_ =
  Prompt'
    { _pResponseCard = Nothing,
      _pMessages = _List1 # pMessages_,
      _pMaxAttempts = _Nat # pMaxAttempts_
    }

-- | A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
pResponseCard :: Lens' Prompt (Maybe Text)
pResponseCard = lens _pResponseCard (\s a -> s {_pResponseCard = a})

-- | An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
pMessages :: Lens' Prompt (NonEmpty Message)
pMessages = lens _pMessages (\s a -> s {_pMessages = a}) . _List1

-- | The number of times to prompt the user for information.
pMaxAttempts :: Lens' Prompt Natural
pMaxAttempts = lens _pMaxAttempts (\s a -> s {_pMaxAttempts = a}) . _Nat

instance FromJSON Prompt where
  parseJSON =
    withObject
      "Prompt"
      ( \x ->
          Prompt'
            <$> (x .:? "responseCard")
            <*> (x .: "messages")
            <*> (x .: "maxAttempts")
      )

instance Hashable Prompt

instance NFData Prompt

instance ToJSON Prompt where
  toJSON Prompt' {..} =
    object
      ( catMaybes
          [ ("responseCard" .=) <$> _pResponseCard,
            Just ("messages" .= _pMessages),
            Just ("maxAttempts" .= _pMaxAttempts)
          ]
      )
