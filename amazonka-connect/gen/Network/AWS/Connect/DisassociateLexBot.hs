{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Revokes authorization from the specified instance to access the
-- specified Amazon Lex bot.
module Network.AWS.Connect.DisassociateLexBot
  ( -- * Creating a Request
    DisassociateLexBot (..),
    newDisassociateLexBot,

    -- * Request Lenses
    disassociateLexBot_instanceId,
    disassociateLexBot_botName,
    disassociateLexBot_lexRegion,

    -- * Destructuring the Response
    DisassociateLexBotResponse (..),
    newDisassociateLexBotResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The name of the Amazon Lex bot. Maximum character limit of 50.
    botName :: Prelude.Text,
    -- | The Region in which the Amazon Lex bot has been created.
    lexRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLexBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateLexBot_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'botName', 'disassociateLexBot_botName' - The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- 'lexRegion', 'disassociateLexBot_lexRegion' - The Region in which the Amazon Lex bot has been created.
newDisassociateLexBot ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  -- | 'lexRegion'
  Prelude.Text ->
  DisassociateLexBot
newDisassociateLexBot
  pInstanceId_
  pBotName_
  pLexRegion_ =
    DisassociateLexBot'
      { instanceId = pInstanceId_,
        botName = pBotName_,
        lexRegion = pLexRegion_
      }

-- | The identifier of the Amazon Connect instance.
disassociateLexBot_instanceId :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_instanceId = Lens.lens (\DisassociateLexBot' {instanceId} -> instanceId) (\s@DisassociateLexBot' {} a -> s {instanceId = a} :: DisassociateLexBot)

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
disassociateLexBot_botName :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_botName = Lens.lens (\DisassociateLexBot' {botName} -> botName) (\s@DisassociateLexBot' {} a -> s {botName = a} :: DisassociateLexBot)

-- | The Region in which the Amazon Lex bot has been created.
disassociateLexBot_lexRegion :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_lexRegion = Lens.lens (\DisassociateLexBot' {lexRegion} -> lexRegion) (\s@DisassociateLexBot' {} a -> s {lexRegion = a} :: DisassociateLexBot)

instance Prelude.AWSRequest DisassociateLexBot where
  type
    Rs DisassociateLexBot =
      DisassociateLexBotResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DisassociateLexBotResponse'

instance Prelude.Hashable DisassociateLexBot

instance Prelude.NFData DisassociateLexBot

instance Prelude.ToHeaders DisassociateLexBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DisassociateLexBot where
  toPath DisassociateLexBot' {..} =
    Prelude.mconcat
      ["/instance/", Prelude.toBS instanceId, "/lex-bot"]

instance Prelude.ToQuery DisassociateLexBot where
  toQuery DisassociateLexBot' {..} =
    Prelude.mconcat
      [ "botName" Prelude.=: botName,
        "lexRegion" Prelude.=: lexRegion
      ]

-- | /See:/ 'newDisassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLexBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateLexBotResponse ::
  DisassociateLexBotResponse
newDisassociateLexBotResponse =
  DisassociateLexBotResponse'

instance Prelude.NFData DisassociateLexBotResponse
