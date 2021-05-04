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
-- Module      : Network.AWS.LexModels.DeleteBotChannelAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between an Amazon Lex bot and a messaging
-- platform.
--
-- This operation requires permission for the
-- @lex:DeleteBotChannelAssociation@ action.
module Network.AWS.LexModels.DeleteBotChannelAssociation
  ( -- * Creating a Request
    DeleteBotChannelAssociation (..),
    newDeleteBotChannelAssociation,

    -- * Request Lenses
    deleteBotChannelAssociation_name,
    deleteBotChannelAssociation_botName,
    deleteBotChannelAssociation_botAlias,

    -- * Destructuring the Response
    DeleteBotChannelAssociationResponse (..),
    newDeleteBotChannelAssociationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBotChannelAssociation' smart constructor.
data DeleteBotChannelAssociation = DeleteBotChannelAssociation'
  { -- | The name of the association. The name is case sensitive.
    name :: Prelude.Text,
    -- | The name of the Amazon Lex bot.
    botName :: Prelude.Text,
    -- | An alias that points to the specific version of the Amazon Lex bot to
    -- which this association is being made.
    botAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotChannelAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBotChannelAssociation_name' - The name of the association. The name is case sensitive.
--
-- 'botName', 'deleteBotChannelAssociation_botName' - The name of the Amazon Lex bot.
--
-- 'botAlias', 'deleteBotChannelAssociation_botAlias' - An alias that points to the specific version of the Amazon Lex bot to
-- which this association is being made.
newDeleteBotChannelAssociation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  DeleteBotChannelAssociation
newDeleteBotChannelAssociation
  pName_
  pBotName_
  pBotAlias_ =
    DeleteBotChannelAssociation'
      { name = pName_,
        botName = pBotName_,
        botAlias = pBotAlias_
      }

-- | The name of the association. The name is case sensitive.
deleteBotChannelAssociation_name :: Lens.Lens' DeleteBotChannelAssociation Prelude.Text
deleteBotChannelAssociation_name = Lens.lens (\DeleteBotChannelAssociation' {name} -> name) (\s@DeleteBotChannelAssociation' {} a -> s {name = a} :: DeleteBotChannelAssociation)

-- | The name of the Amazon Lex bot.
deleteBotChannelAssociation_botName :: Lens.Lens' DeleteBotChannelAssociation Prelude.Text
deleteBotChannelAssociation_botName = Lens.lens (\DeleteBotChannelAssociation' {botName} -> botName) (\s@DeleteBotChannelAssociation' {} a -> s {botName = a} :: DeleteBotChannelAssociation)

-- | An alias that points to the specific version of the Amazon Lex bot to
-- which this association is being made.
deleteBotChannelAssociation_botAlias :: Lens.Lens' DeleteBotChannelAssociation Prelude.Text
deleteBotChannelAssociation_botAlias = Lens.lens (\DeleteBotChannelAssociation' {botAlias} -> botAlias) (\s@DeleteBotChannelAssociation' {} a -> s {botAlias = a} :: DeleteBotChannelAssociation)

instance
  Prelude.AWSRequest
    DeleteBotChannelAssociation
  where
  type
    Rs DeleteBotChannelAssociation =
      DeleteBotChannelAssociationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBotChannelAssociationResponse'

instance Prelude.Hashable DeleteBotChannelAssociation

instance Prelude.NFData DeleteBotChannelAssociation

instance
  Prelude.ToHeaders
    DeleteBotChannelAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteBotChannelAssociation where
  toPath DeleteBotChannelAssociation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Prelude.toBS botName,
        "/aliases/",
        Prelude.toBS botAlias,
        "/channels/",
        Prelude.toBS name
      ]

instance Prelude.ToQuery DeleteBotChannelAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotChannelAssociationResponse' smart constructor.
data DeleteBotChannelAssociationResponse = DeleteBotChannelAssociationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotChannelAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotChannelAssociationResponse ::
  DeleteBotChannelAssociationResponse
newDeleteBotChannelAssociationResponse =
  DeleteBotChannelAssociationResponse'

instance
  Prelude.NFData
    DeleteBotChannelAssociationResponse
