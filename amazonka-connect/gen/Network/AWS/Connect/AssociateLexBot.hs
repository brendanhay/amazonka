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
-- Module      : Network.AWS.Connect.AssociateLexBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Allows the specified Amazon Connect instance to access the specified
-- Amazon Lex bot.
module Network.AWS.Connect.AssociateLexBot
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateLexBot' smart constructor.
data AssociateLexBot = AssociateLexBot'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The Amazon Lex box to associate with the instance.
    lexBot :: LexBot
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateLexBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateLexBot_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'lexBot', 'associateLexBot_lexBot' - The Amazon Lex box to associate with the instance.
newAssociateLexBot ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'lexBot'
  LexBot ->
  AssociateLexBot
newAssociateLexBot pInstanceId_ pLexBot_ =
  AssociateLexBot'
    { instanceId = pInstanceId_,
      lexBot = pLexBot_
    }

-- | The identifier of the Amazon Connect instance.
associateLexBot_instanceId :: Lens.Lens' AssociateLexBot Core.Text
associateLexBot_instanceId = Lens.lens (\AssociateLexBot' {instanceId} -> instanceId) (\s@AssociateLexBot' {} a -> s {instanceId = a} :: AssociateLexBot)

-- | The Amazon Lex box to associate with the instance.
associateLexBot_lexBot :: Lens.Lens' AssociateLexBot LexBot
associateLexBot_lexBot = Lens.lens (\AssociateLexBot' {lexBot} -> lexBot) (\s@AssociateLexBot' {} a -> s {lexBot = a} :: AssociateLexBot)

instance Core.AWSRequest AssociateLexBot where
  type
    AWSResponse AssociateLexBot =
      AssociateLexBotResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull AssociateLexBotResponse'

instance Core.Hashable AssociateLexBot

instance Core.NFData AssociateLexBot

instance Core.ToHeaders AssociateLexBot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateLexBot where
  toJSON AssociateLexBot' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("LexBot" Core..= lexBot)]
      )

instance Core.ToPath AssociateLexBot where
  toPath AssociateLexBot' {..} =
    Core.mconcat
      ["/instance/", Core.toBS instanceId, "/lex-bot"]

instance Core.ToQuery AssociateLexBot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateLexBotResponse' smart constructor.
data AssociateLexBotResponse = AssociateLexBotResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateLexBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateLexBotResponse ::
  AssociateLexBotResponse
newAssociateLexBotResponse = AssociateLexBotResponse'

instance Core.NFData AssociateLexBotResponse
