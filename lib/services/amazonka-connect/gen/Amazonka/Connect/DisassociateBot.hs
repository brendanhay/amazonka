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
-- Module      : Amazonka.Connect.DisassociateBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Revokes authorization from the specified instance to access the
-- specified Amazon Lex or Amazon Lex V2 bot.
module Amazonka.Connect.DisassociateBot
  ( -- * Creating a Request
    DisassociateBot (..),
    newDisassociateBot,

    -- * Request Lenses
    disassociateBot_lexBot,
    disassociateBot_lexV2Bot,
    disassociateBot_instanceId,

    -- * Destructuring the Response
    DisassociateBotResponse (..),
    newDisassociateBotResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateBot' smart constructor.
data DisassociateBot = DisassociateBot'
  { lexBot :: Prelude.Maybe LexBot,
    -- | The Amazon Lex V2 bot to disassociate from the instance.
    lexV2Bot :: Prelude.Maybe LexV2Bot,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexBot', 'disassociateBot_lexBot' - Undocumented member.
--
-- 'lexV2Bot', 'disassociateBot_lexV2Bot' - The Amazon Lex V2 bot to disassociate from the instance.
--
-- 'instanceId', 'disassociateBot_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newDisassociateBot ::
  -- | 'instanceId'
  Prelude.Text ->
  DisassociateBot
newDisassociateBot pInstanceId_ =
  DisassociateBot'
    { lexBot = Prelude.Nothing,
      lexV2Bot = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Undocumented member.
disassociateBot_lexBot :: Lens.Lens' DisassociateBot (Prelude.Maybe LexBot)
disassociateBot_lexBot = Lens.lens (\DisassociateBot' {lexBot} -> lexBot) (\s@DisassociateBot' {} a -> s {lexBot = a} :: DisassociateBot)

-- | The Amazon Lex V2 bot to disassociate from the instance.
disassociateBot_lexV2Bot :: Lens.Lens' DisassociateBot (Prelude.Maybe LexV2Bot)
disassociateBot_lexV2Bot = Lens.lens (\DisassociateBot' {lexV2Bot} -> lexV2Bot) (\s@DisassociateBot' {} a -> s {lexV2Bot = a} :: DisassociateBot)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
disassociateBot_instanceId :: Lens.Lens' DisassociateBot Prelude.Text
disassociateBot_instanceId = Lens.lens (\DisassociateBot' {instanceId} -> instanceId) (\s@DisassociateBot' {} a -> s {instanceId = a} :: DisassociateBot)

instance Core.AWSRequest DisassociateBot where
  type
    AWSResponse DisassociateBot =
      DisassociateBotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DisassociateBotResponse'

instance Prelude.Hashable DisassociateBot where
  hashWithSalt _salt DisassociateBot' {..} =
    _salt
      `Prelude.hashWithSalt` lexBot
      `Prelude.hashWithSalt` lexV2Bot
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DisassociateBot where
  rnf DisassociateBot' {..} =
    Prelude.rnf lexBot
      `Prelude.seq` Prelude.rnf lexV2Bot
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders DisassociateBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateBot where
  toJSON DisassociateBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LexBot" Data..=) Prelude.<$> lexBot,
            ("LexV2Bot" Data..=) Prelude.<$> lexV2Bot
          ]
      )

instance Data.ToPath DisassociateBot where
  toPath DisassociateBot' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/bot"]

instance Data.ToQuery DisassociateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateBotResponse' smart constructor.
data DisassociateBotResponse = DisassociateBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateBotResponse ::
  DisassociateBotResponse
newDisassociateBotResponse = DisassociateBotResponse'

instance Prelude.NFData DisassociateBotResponse where
  rnf _ = ()
