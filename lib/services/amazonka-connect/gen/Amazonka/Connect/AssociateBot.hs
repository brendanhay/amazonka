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
-- Module      : Amazonka.Connect.AssociateBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Allows the specified Amazon Connect instance to access the specified
-- Amazon Lex or Amazon Lex V2 bot.
module Amazonka.Connect.AssociateBot
  ( -- * Creating a Request
    AssociateBot (..),
    newAssociateBot,

    -- * Request Lenses
    associateBot_lexBot,
    associateBot_lexV2Bot,
    associateBot_instanceId,

    -- * Destructuring the Response
    AssociateBotResponse (..),
    newAssociateBotResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateBot' smart constructor.
data AssociateBot = AssociateBot'
  { lexBot :: Prelude.Maybe LexBot,
    -- | The Amazon Lex V2 bot to associate with the instance.
    lexV2Bot :: Prelude.Maybe LexV2Bot,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexBot', 'associateBot_lexBot' - Undocumented member.
--
-- 'lexV2Bot', 'associateBot_lexV2Bot' - The Amazon Lex V2 bot to associate with the instance.
--
-- 'instanceId', 'associateBot_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newAssociateBot ::
  -- | 'instanceId'
  Prelude.Text ->
  AssociateBot
newAssociateBot pInstanceId_ =
  AssociateBot'
    { lexBot = Prelude.Nothing,
      lexV2Bot = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Undocumented member.
associateBot_lexBot :: Lens.Lens' AssociateBot (Prelude.Maybe LexBot)
associateBot_lexBot = Lens.lens (\AssociateBot' {lexBot} -> lexBot) (\s@AssociateBot' {} a -> s {lexBot = a} :: AssociateBot)

-- | The Amazon Lex V2 bot to associate with the instance.
associateBot_lexV2Bot :: Lens.Lens' AssociateBot (Prelude.Maybe LexV2Bot)
associateBot_lexV2Bot = Lens.lens (\AssociateBot' {lexV2Bot} -> lexV2Bot) (\s@AssociateBot' {} a -> s {lexV2Bot = a} :: AssociateBot)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
associateBot_instanceId :: Lens.Lens' AssociateBot Prelude.Text
associateBot_instanceId = Lens.lens (\AssociateBot' {instanceId} -> instanceId) (\s@AssociateBot' {} a -> s {instanceId = a} :: AssociateBot)

instance Core.AWSRequest AssociateBot where
  type AWSResponse AssociateBot = AssociateBotResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response = Response.receiveNull AssociateBotResponse'

instance Prelude.Hashable AssociateBot where
  hashWithSalt _salt AssociateBot' {..} =
    _salt `Prelude.hashWithSalt` lexBot
      `Prelude.hashWithSalt` lexV2Bot
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData AssociateBot where
  rnf AssociateBot' {..} =
    Prelude.rnf lexBot
      `Prelude.seq` Prelude.rnf lexV2Bot
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders AssociateBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateBot where
  toJSON AssociateBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LexBot" Data..=) Prelude.<$> lexBot,
            ("LexV2Bot" Data..=) Prelude.<$> lexV2Bot
          ]
      )

instance Data.ToPath AssociateBot where
  toPath AssociateBot' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/bot"]

instance Data.ToQuery AssociateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateBotResponse' smart constructor.
data AssociateBotResponse = AssociateBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateBotResponse ::
  AssociateBotResponse
newAssociateBotResponse = AssociateBotResponse'

instance Prelude.NFData AssociateBotResponse where
  rnf _ = ()
