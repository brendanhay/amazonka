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
-- Module      : Network.AWS.LexModels.DeleteBotAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias for the specified bot.
--
-- You can\'t delete an alias that is used in the association between a bot
-- and a messaging channel. If an alias is used in a channel association,
-- the @DeleteBot@ operation returns a @ResourceInUseException@ exception
-- that includes a reference to the channel association that refers to the
-- bot. You can remove the reference to the alias by deleting the channel
-- association. If you get the same exception again, delete the referring
-- association until the @DeleteBotAlias@ operation is successful.
module Network.AWS.LexModels.DeleteBotAlias
  ( -- * Creating a Request
    DeleteBotAlias (..),
    newDeleteBotAlias,

    -- * Request Lenses
    deleteBotAlias_name,
    deleteBotAlias_botName,

    -- * Destructuring the Response
    DeleteBotAliasResponse (..),
    newDeleteBotAliasResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBotAlias' smart constructor.
data DeleteBotAlias = DeleteBotAlias'
  { -- | The name of the alias to delete. The name is case sensitive.
    name :: Prelude.Text,
    -- | The name of the bot that the alias points to.
    botName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBotAlias_name' - The name of the alias to delete. The name is case sensitive.
--
-- 'botName', 'deleteBotAlias_botName' - The name of the bot that the alias points to.
newDeleteBotAlias ::
  -- | 'name'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  DeleteBotAlias
newDeleteBotAlias pName_ pBotName_ =
  DeleteBotAlias' {name = pName_, botName = pBotName_}

-- | The name of the alias to delete. The name is case sensitive.
deleteBotAlias_name :: Lens.Lens' DeleteBotAlias Prelude.Text
deleteBotAlias_name = Lens.lens (\DeleteBotAlias' {name} -> name) (\s@DeleteBotAlias' {} a -> s {name = a} :: DeleteBotAlias)

-- | The name of the bot that the alias points to.
deleteBotAlias_botName :: Lens.Lens' DeleteBotAlias Prelude.Text
deleteBotAlias_botName = Lens.lens (\DeleteBotAlias' {botName} -> botName) (\s@DeleteBotAlias' {} a -> s {botName = a} :: DeleteBotAlias)

instance Prelude.AWSRequest DeleteBotAlias where
  type Rs DeleteBotAlias = DeleteBotAliasResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBotAliasResponse'

instance Prelude.Hashable DeleteBotAlias

instance Prelude.NFData DeleteBotAlias

instance Prelude.ToHeaders DeleteBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteBotAlias where
  toPath DeleteBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Prelude.toBS botName,
        "/aliases/",
        Prelude.toBS name
      ]

instance Prelude.ToQuery DeleteBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotAliasResponse' smart constructor.
data DeleteBotAliasResponse = DeleteBotAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotAliasResponse ::
  DeleteBotAliasResponse
newDeleteBotAliasResponse = DeleteBotAliasResponse'

instance Prelude.NFData DeleteBotAliasResponse
