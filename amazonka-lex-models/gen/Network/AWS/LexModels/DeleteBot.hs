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
-- Module      : Network.AWS.LexModels.DeleteBot
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LexModels.DeleteBot
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBot' smart constructor.
data DeleteBot = DeleteBot'
  { -- | The name of the bot. The name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteBot where
  type Rs DeleteBot = DeleteBotResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteBotResponse'

instance Prelude.Hashable DeleteBot

instance Prelude.NFData DeleteBot

instance Prelude.ToHeaders DeleteBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteBot where
  toPath DeleteBot' {..} =
    Prelude.mconcat ["/bots/", Prelude.toBS name]

instance Prelude.ToQuery DeleteBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotResponse' smart constructor.
data DeleteBotResponse = DeleteBotResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotResponse ::
  DeleteBotResponse
newDeleteBotResponse = DeleteBotResponse'

instance Prelude.NFData DeleteBotResponse
