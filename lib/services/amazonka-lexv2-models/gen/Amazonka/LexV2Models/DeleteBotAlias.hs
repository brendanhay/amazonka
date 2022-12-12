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
-- Module      : Amazonka.LexV2Models.DeleteBotAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified bot alias.
module Amazonka.LexV2Models.DeleteBotAlias
  ( -- * Creating a Request
    DeleteBotAlias (..),
    newDeleteBotAlias,

    -- * Request Lenses
    deleteBotAlias_skipResourceInUseCheck,
    deleteBotAlias_botAliasId,
    deleteBotAlias_botId,

    -- * Destructuring the Response
    DeleteBotAliasResponse (..),
    newDeleteBotAliasResponse,

    -- * Response Lenses
    deleteBotAliasResponse_botAliasId,
    deleteBotAliasResponse_botAliasStatus,
    deleteBotAliasResponse_botId,
    deleteBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBotAlias' smart constructor.
data DeleteBotAlias = DeleteBotAlias'
  { -- | When this parameter is true, Amazon Lex doesn\'t check to see if any
    -- other resource is using the alias before it is deleted.
    skipResourceInUseCheck :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the bot alias to delete.
    botAliasId :: Prelude.Text,
    -- | The unique identifier of the bot associated with the alias to delete.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipResourceInUseCheck', 'deleteBotAlias_skipResourceInUseCheck' - When this parameter is true, Amazon Lex doesn\'t check to see if any
-- other resource is using the alias before it is deleted.
--
-- 'botAliasId', 'deleteBotAlias_botAliasId' - The unique identifier of the bot alias to delete.
--
-- 'botId', 'deleteBotAlias_botId' - The unique identifier of the bot associated with the alias to delete.
newDeleteBotAlias ::
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  DeleteBotAlias
newDeleteBotAlias pBotAliasId_ pBotId_ =
  DeleteBotAlias'
    { skipResourceInUseCheck =
        Prelude.Nothing,
      botAliasId = pBotAliasId_,
      botId = pBotId_
    }

-- | When this parameter is true, Amazon Lex doesn\'t check to see if any
-- other resource is using the alias before it is deleted.
deleteBotAlias_skipResourceInUseCheck :: Lens.Lens' DeleteBotAlias (Prelude.Maybe Prelude.Bool)
deleteBotAlias_skipResourceInUseCheck = Lens.lens (\DeleteBotAlias' {skipResourceInUseCheck} -> skipResourceInUseCheck) (\s@DeleteBotAlias' {} a -> s {skipResourceInUseCheck = a} :: DeleteBotAlias)

-- | The unique identifier of the bot alias to delete.
deleteBotAlias_botAliasId :: Lens.Lens' DeleteBotAlias Prelude.Text
deleteBotAlias_botAliasId = Lens.lens (\DeleteBotAlias' {botAliasId} -> botAliasId) (\s@DeleteBotAlias' {} a -> s {botAliasId = a} :: DeleteBotAlias)

-- | The unique identifier of the bot associated with the alias to delete.
deleteBotAlias_botId :: Lens.Lens' DeleteBotAlias Prelude.Text
deleteBotAlias_botId = Lens.lens (\DeleteBotAlias' {botId} -> botId) (\s@DeleteBotAlias' {} a -> s {botId = a} :: DeleteBotAlias)

instance Core.AWSRequest DeleteBotAlias where
  type
    AWSResponse DeleteBotAlias =
      DeleteBotAliasResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotAliasResponse'
            Prelude.<$> (x Data..?> "botAliasId")
            Prelude.<*> (x Data..?> "botAliasStatus")
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBotAlias where
  hashWithSalt _salt DeleteBotAlias' {..} =
    _salt `Prelude.hashWithSalt` skipResourceInUseCheck
      `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData DeleteBotAlias where
  rnf DeleteBotAlias' {..} =
    Prelude.rnf skipResourceInUseCheck
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders DeleteBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBotAlias where
  toPath DeleteBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botaliases/",
        Data.toBS botAliasId,
        "/"
      ]

instance Data.ToQuery DeleteBotAlias where
  toQuery DeleteBotAlias' {..} =
    Prelude.mconcat
      [ "skipResourceInUseCheck"
          Data.=: skipResourceInUseCheck
      ]

-- | /See:/ 'newDeleteBotAliasResponse' smart constructor.
data DeleteBotAliasResponse = DeleteBotAliasResponse'
  { -- | The unique identifier of the bot alias to delete.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the alias. The status is @Deleting@ while the
    -- alias is in the process of being deleted. Once the alias is deleted, it
    -- will no longer appear in the list of aliases returned by the
    -- @ListBotAliases@ operation.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The unique identifier of the bot that contains the alias to delete.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasId', 'deleteBotAliasResponse_botAliasId' - The unique identifier of the bot alias to delete.
--
-- 'botAliasStatus', 'deleteBotAliasResponse_botAliasStatus' - The current status of the alias. The status is @Deleting@ while the
-- alias is in the process of being deleted. Once the alias is deleted, it
-- will no longer appear in the list of aliases returned by the
-- @ListBotAliases@ operation.
--
-- 'botId', 'deleteBotAliasResponse_botId' - The unique identifier of the bot that contains the alias to delete.
--
-- 'httpStatus', 'deleteBotAliasResponse_httpStatus' - The response's http status code.
newDeleteBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotAliasResponse
newDeleteBotAliasResponse pHttpStatus_ =
  DeleteBotAliasResponse'
    { botAliasId =
        Prelude.Nothing,
      botAliasStatus = Prelude.Nothing,
      botId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot alias to delete.
deleteBotAliasResponse_botAliasId :: Lens.Lens' DeleteBotAliasResponse (Prelude.Maybe Prelude.Text)
deleteBotAliasResponse_botAliasId = Lens.lens (\DeleteBotAliasResponse' {botAliasId} -> botAliasId) (\s@DeleteBotAliasResponse' {} a -> s {botAliasId = a} :: DeleteBotAliasResponse)

-- | The current status of the alias. The status is @Deleting@ while the
-- alias is in the process of being deleted. Once the alias is deleted, it
-- will no longer appear in the list of aliases returned by the
-- @ListBotAliases@ operation.
deleteBotAliasResponse_botAliasStatus :: Lens.Lens' DeleteBotAliasResponse (Prelude.Maybe BotAliasStatus)
deleteBotAliasResponse_botAliasStatus = Lens.lens (\DeleteBotAliasResponse' {botAliasStatus} -> botAliasStatus) (\s@DeleteBotAliasResponse' {} a -> s {botAliasStatus = a} :: DeleteBotAliasResponse)

-- | The unique identifier of the bot that contains the alias to delete.
deleteBotAliasResponse_botId :: Lens.Lens' DeleteBotAliasResponse (Prelude.Maybe Prelude.Text)
deleteBotAliasResponse_botId = Lens.lens (\DeleteBotAliasResponse' {botId} -> botId) (\s@DeleteBotAliasResponse' {} a -> s {botId = a} :: DeleteBotAliasResponse)

-- | The response's http status code.
deleteBotAliasResponse_httpStatus :: Lens.Lens' DeleteBotAliasResponse Prelude.Int
deleteBotAliasResponse_httpStatus = Lens.lens (\DeleteBotAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteBotAliasResponse' {} a -> s {httpStatus = a} :: DeleteBotAliasResponse)

instance Prelude.NFData DeleteBotAliasResponse where
  rnf DeleteBotAliasResponse' {..} =
    Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botAliasStatus
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf httpStatus
