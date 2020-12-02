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
-- Module      : Network.AWS.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes authorization from the specified instance to access the specified Amazon Lex bot.
module Network.AWS.Connect.DisassociateLexBot
  ( -- * Creating a Request
    disassociateLexBot,
    DisassociateLexBot,

    -- * Request Lenses
    dlbInstanceId,
    dlbBotName,
    dlbLexRegion,

    -- * Destructuring the Response
    disassociateLexBotResponse,
    DisassociateLexBotResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { _dlbInstanceId ::
      !Text,
    _dlbBotName :: !Text,
    _dlbLexRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateLexBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'dlbBotName' - The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- * 'dlbLexRegion' - The Region in which the Amazon Lex bot has been created.
disassociateLexBot ::
  -- | 'dlbInstanceId'
  Text ->
  -- | 'dlbBotName'
  Text ->
  -- | 'dlbLexRegion'
  Text ->
  DisassociateLexBot
disassociateLexBot pInstanceId_ pBotName_ pLexRegion_ =
  DisassociateLexBot'
    { _dlbInstanceId = pInstanceId_,
      _dlbBotName = pBotName_,
      _dlbLexRegion = pLexRegion_
    }

-- | The identifier of the Amazon Connect instance.
dlbInstanceId :: Lens' DisassociateLexBot Text
dlbInstanceId = lens _dlbInstanceId (\s a -> s {_dlbInstanceId = a})

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
dlbBotName :: Lens' DisassociateLexBot Text
dlbBotName = lens _dlbBotName (\s a -> s {_dlbBotName = a})

-- | The Region in which the Amazon Lex bot has been created.
dlbLexRegion :: Lens' DisassociateLexBot Text
dlbLexRegion = lens _dlbLexRegion (\s a -> s {_dlbLexRegion = a})

instance AWSRequest DisassociateLexBot where
  type Rs DisassociateLexBot = DisassociateLexBotResponse
  request = delete connect
  response = receiveNull DisassociateLexBotResponse'

instance Hashable DisassociateLexBot

instance NFData DisassociateLexBot

instance ToHeaders DisassociateLexBot where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DisassociateLexBot where
  toPath DisassociateLexBot' {..} =
    mconcat ["/instance/", toBS _dlbInstanceId, "/lex-bot"]

instance ToQuery DisassociateLexBot where
  toQuery DisassociateLexBot' {..} =
    mconcat ["botName" =: _dlbBotName, "lexRegion" =: _dlbLexRegion]

-- | /See:/ 'disassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateLexBotResponse' with the minimum fields required to make a request.
disassociateLexBotResponse ::
  DisassociateLexBotResponse
disassociateLexBotResponse = DisassociateLexBotResponse'

instance NFData DisassociateLexBotResponse
