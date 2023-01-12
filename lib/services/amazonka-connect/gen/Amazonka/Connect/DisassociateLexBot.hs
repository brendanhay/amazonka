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
-- Module      : Amazonka.Connect.DisassociateLexBot
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Connect.DisassociateLexBot
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateLexBot' smart constructor.
data DisassociateLexBot = DisassociateLexBot'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The name of the Amazon Lex bot. Maximum character limit of 50.
    botName :: Prelude.Text,
    -- | The Amazon Web Services Region in which the Amazon Lex bot has been
    -- created.
    lexRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLexBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateLexBot_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'botName', 'disassociateLexBot_botName' - The name of the Amazon Lex bot. Maximum character limit of 50.
--
-- 'lexRegion', 'disassociateLexBot_lexRegion' - The Amazon Web Services Region in which the Amazon Lex bot has been
-- created.
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociateLexBot_instanceId :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_instanceId = Lens.lens (\DisassociateLexBot' {instanceId} -> instanceId) (\s@DisassociateLexBot' {} a -> s {instanceId = a} :: DisassociateLexBot)

-- | The name of the Amazon Lex bot. Maximum character limit of 50.
disassociateLexBot_botName :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_botName = Lens.lens (\DisassociateLexBot' {botName} -> botName) (\s@DisassociateLexBot' {} a -> s {botName = a} :: DisassociateLexBot)

-- | The Amazon Web Services Region in which the Amazon Lex bot has been
-- created.
disassociateLexBot_lexRegion :: Lens.Lens' DisassociateLexBot Prelude.Text
disassociateLexBot_lexRegion = Lens.lens (\DisassociateLexBot' {lexRegion} -> lexRegion) (\s@DisassociateLexBot' {} a -> s {lexRegion = a} :: DisassociateLexBot)

instance Core.AWSRequest DisassociateLexBot where
  type
    AWSResponse DisassociateLexBot =
      DisassociateLexBotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DisassociateLexBotResponse'

instance Prelude.Hashable DisassociateLexBot where
  hashWithSalt _salt DisassociateLexBot' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` lexRegion

instance Prelude.NFData DisassociateLexBot where
  rnf DisassociateLexBot' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf lexRegion

instance Data.ToHeaders DisassociateLexBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateLexBot where
  toPath DisassociateLexBot' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/lex-bot"]

instance Data.ToQuery DisassociateLexBot where
  toQuery DisassociateLexBot' {..} =
    Prelude.mconcat
      [ "botName" Data.=: botName,
        "lexRegion" Data.=: lexRegion
      ]

-- | /See:/ 'newDisassociateLexBotResponse' smart constructor.
data DisassociateLexBotResponse = DisassociateLexBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLexBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateLexBotResponse ::
  DisassociateLexBotResponse
newDisassociateLexBotResponse =
  DisassociateLexBotResponse'

instance Prelude.NFData DisassociateLexBotResponse where
  rnf _ = ()
