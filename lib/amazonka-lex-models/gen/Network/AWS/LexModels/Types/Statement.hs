{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Statement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Statement where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Message
import Network.AWS.Prelude

-- | A collection of messages that convey information to the user. At runtime, Amazon Lex selects the message to convey.
--
--
--
-- /See:/ 'statement' smart constructor.
data Statement = Statement'
  { _staResponseCard :: !(Maybe Text),
    _staMessages :: !(List1 Message)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staResponseCard' - At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
--
-- * 'staMessages' - A collection of message objects.
statement ::
  -- | 'staMessages'
  NonEmpty Message ->
  Statement
statement pMessages_ =
  Statement'
    { _staResponseCard = Nothing,
      _staMessages = _List1 # pMessages_
    }

-- | At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
staResponseCard :: Lens' Statement (Maybe Text)
staResponseCard = lens _staResponseCard (\s a -> s {_staResponseCard = a})

-- | A collection of message objects.
staMessages :: Lens' Statement (NonEmpty Message)
staMessages = lens _staMessages (\s a -> s {_staMessages = a}) . _List1

instance FromJSON Statement where
  parseJSON =
    withObject
      "Statement"
      ( \x ->
          Statement' <$> (x .:? "responseCard") <*> (x .: "messages")
      )

instance Hashable Statement

instance NFData Statement

instance ToJSON Statement where
  toJSON Statement' {..} =
    object
      ( catMaybes
          [ ("responseCard" .=) <$> _staResponseCard,
            Just ("messages" .= _staMessages)
          ]
      )
