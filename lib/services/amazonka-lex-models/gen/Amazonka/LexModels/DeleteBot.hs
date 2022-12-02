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
-- Module      : Amazonka.LexModels.DeleteBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the bot, including the @$LATEST@ version. To
-- delete a specific version of the bot, use the DeleteBotVersion
-- operation. The @DeleteBot@ operation doesn\'t immediately remove the bot
-- schema. Instead, it is marked for deletion and removed later.
--
-- Amazon Lex stores utterances indefinitely for improving the ability of
-- your bot to respond to user inputs. These utterances are not removed
-- when the bot is deleted. To remove the utterances, use the
-- DeleteUtterances operation.
--
-- If a bot has an alias, you can\'t delete it. Instead, the @DeleteBot@
-- operation returns a @ResourceInUseException@ exception that includes a
-- reference to the alias that refers to the bot. To remove the reference
-- to the bot, delete the alias. If you get the same exception again,
-- delete the referring alias until the @DeleteBot@ operation is
-- successful.
--
-- This operation requires permissions for the @lex:DeleteBot@ action.
module Amazonka.LexModels.DeleteBot
  ( -- * Creating a Request
    DeleteBot (..),
    newDeleteBot,

    -- * Request Lenses
    deleteBot_name,

    -- * Destructuring the Response
    DeleteBotResponse (..),
    newDeleteBotResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBot' smart constructor.
data DeleteBot = DeleteBot'
  { -- | The name of the bot. The name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBot_name' - The name of the bot. The name is case sensitive.
newDeleteBot ::
  -- | 'name'
  Prelude.Text ->
  DeleteBot
newDeleteBot pName_ = DeleteBot' {name = pName_}

-- | The name of the bot. The name is case sensitive.
deleteBot_name :: Lens.Lens' DeleteBot Prelude.Text
deleteBot_name = Lens.lens (\DeleteBot' {name} -> name) (\s@DeleteBot' {} a -> s {name = a} :: DeleteBot)

instance Core.AWSRequest DeleteBot where
  type AWSResponse DeleteBot = DeleteBotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteBotResponse'

instance Prelude.Hashable DeleteBot where
  hashWithSalt _salt DeleteBot' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteBot where
  rnf DeleteBot' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBot where
  toPath DeleteBot' {..} =
    Prelude.mconcat ["/bots/", Data.toBS name]

instance Data.ToQuery DeleteBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotResponse' smart constructor.
data DeleteBotResponse = DeleteBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotResponse ::
  DeleteBotResponse
newDeleteBotResponse = DeleteBotResponse'

instance Prelude.NFData DeleteBotResponse where
  rnf _ = ()
