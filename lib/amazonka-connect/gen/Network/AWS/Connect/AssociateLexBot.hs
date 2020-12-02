{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.AssociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.AssociateLexBot
  ( -- * Creating a Request
    associateLexBot,
    AssociateLexBot,

    -- * Request Lenses
    albInstanceId,
    albLexBot,

    -- * Destructuring the Response
    associateLexBotResponse,
    AssociateLexBotResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateLexBot' smart constructor.
data AssociateLexBot = AssociateLexBot'
  { _albInstanceId :: !Text,
    _albLexBot :: !LexBot
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateLexBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'albLexBot' - The Amazon Lex box to associate with the instance.
associateLexBot ::
  -- | 'albInstanceId'
  Text ->
  -- | 'albLexBot'
  LexBot ->
  AssociateLexBot
associateLexBot pInstanceId_ pLexBot_ =
  AssociateLexBot'
    { _albInstanceId = pInstanceId_,
      _albLexBot = pLexBot_
    }

-- | The identifier of the Amazon Connect instance.
albInstanceId :: Lens' AssociateLexBot Text
albInstanceId = lens _albInstanceId (\s a -> s {_albInstanceId = a})

-- | The Amazon Lex box to associate with the instance.
albLexBot :: Lens' AssociateLexBot LexBot
albLexBot = lens _albLexBot (\s a -> s {_albLexBot = a})

instance AWSRequest AssociateLexBot where
  type Rs AssociateLexBot = AssociateLexBotResponse
  request = putJSON connect
  response = receiveNull AssociateLexBotResponse'

instance Hashable AssociateLexBot

instance NFData AssociateLexBot

instance ToHeaders AssociateLexBot where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateLexBot where
  toJSON AssociateLexBot' {..} =
    object (catMaybes [Just ("LexBot" .= _albLexBot)])

instance ToPath AssociateLexBot where
  toPath AssociateLexBot' {..} =
    mconcat ["/instance/", toBS _albInstanceId, "/lex-bot"]

instance ToQuery AssociateLexBot where
  toQuery = const mempty

-- | /See:/ 'associateLexBotResponse' smart constructor.
data AssociateLexBotResponse = AssociateLexBotResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateLexBotResponse' with the minimum fields required to make a request.
associateLexBotResponse ::
  AssociateLexBotResponse
associateLexBotResponse = AssociateLexBotResponse'

instance NFData AssociateLexBotResponse
