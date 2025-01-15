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
-- Module      : Amazonka.Connect.AssociateLexBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Allows the specified Amazon Connect instance to access the specified
-- Amazon Lex bot.
module Amazonka.Connect.AssociateLexBot
  ( -- * Creating a Request
    AssociateLexBot (..),
    newAssociateLexBot,

    -- * Request Lenses
    associateLexBot_instanceId,
    associateLexBot_lexBot,

    -- * Destructuring the Response
    AssociateLexBotResponse (..),
    newAssociateLexBotResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateLexBot' smart constructor.
data AssociateLexBot = AssociateLexBot'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The Amazon Lex bot to associate with the instance.
    lexBot :: LexBot
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLexBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateLexBot_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'lexBot', 'associateLexBot_lexBot' - The Amazon Lex bot to associate with the instance.
newAssociateLexBot ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'lexBot'
  LexBot ->
  AssociateLexBot
newAssociateLexBot pInstanceId_ pLexBot_ =
  AssociateLexBot'
    { instanceId = pInstanceId_,
      lexBot = pLexBot_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
associateLexBot_instanceId :: Lens.Lens' AssociateLexBot Prelude.Text
associateLexBot_instanceId = Lens.lens (\AssociateLexBot' {instanceId} -> instanceId) (\s@AssociateLexBot' {} a -> s {instanceId = a} :: AssociateLexBot)

-- | The Amazon Lex bot to associate with the instance.
associateLexBot_lexBot :: Lens.Lens' AssociateLexBot LexBot
associateLexBot_lexBot = Lens.lens (\AssociateLexBot' {lexBot} -> lexBot) (\s@AssociateLexBot' {} a -> s {lexBot = a} :: AssociateLexBot)

instance Core.AWSRequest AssociateLexBot where
  type
    AWSResponse AssociateLexBot =
      AssociateLexBotResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull AssociateLexBotResponse'

instance Prelude.Hashable AssociateLexBot where
  hashWithSalt _salt AssociateLexBot' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` lexBot

instance Prelude.NFData AssociateLexBot where
  rnf AssociateLexBot' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf lexBot

instance Data.ToHeaders AssociateLexBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateLexBot where
  toJSON AssociateLexBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LexBot" Data..= lexBot)]
      )

instance Data.ToPath AssociateLexBot where
  toPath AssociateLexBot' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/lex-bot"]

instance Data.ToQuery AssociateLexBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateLexBotResponse' smart constructor.
data AssociateLexBotResponse = AssociateLexBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLexBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateLexBotResponse ::
  AssociateLexBotResponse
newAssociateLexBotResponse = AssociateLexBotResponse'

instance Prelude.NFData AssociateLexBotResponse where
  rnf _ = ()
