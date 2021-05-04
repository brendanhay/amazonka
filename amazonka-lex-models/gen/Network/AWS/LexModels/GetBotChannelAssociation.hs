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
-- Module      : Network.AWS.LexModels.GetBotChannelAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the association between an Amazon Lex bot and
-- a messaging platform.
--
-- This operation requires permissions for the
-- @lex:GetBotChannelAssociation@ action.
module Network.AWS.LexModels.GetBotChannelAssociation
  ( -- * Creating a Request
    GetBotChannelAssociation (..),
    newGetBotChannelAssociation,

    -- * Request Lenses
    getBotChannelAssociation_name,
    getBotChannelAssociation_botName,
    getBotChannelAssociation_botAlias,

    -- * Destructuring the Response
    GetBotChannelAssociationResponse (..),
    newGetBotChannelAssociationResponse,

    -- * Response Lenses
    getBotChannelAssociationResponse_botAlias,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotChannelAssociation' smart constructor.
data GetBotChannelAssociation = GetBotChannelAssociation'
  { -- | The name of the association between the bot and the channel. The name is
    -- case sensitive.
    name :: Prelude.Text,
    -- | The name of the Amazon Lex bot.
    botName :: Prelude.Text,
    -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBotChannelAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getBotChannelAssociation_name' - The name of the association between the bot and the channel. The name is
-- case sensitive.
--
-- 'botName', 'getBotChannelAssociation_botName' - The name of the Amazon Lex bot.
--
-- 'botAlias', 'getBotChannelAssociation_botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
newGetBotChannelAssociation ::
  -- | 'name'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  -- | 'botAlias'
  Prelude.Text ->
  GetBotChannelAssociation
newGetBotChannelAssociation
  pName_
  pBotName_
  pBotAlias_ =
    GetBotChannelAssociation'
      { name = pName_,
        botName = pBotName_,
        botAlias = pBotAlias_
      }

-- | The name of the association between the bot and the channel. The name is
-- case sensitive.
getBotChannelAssociation_name :: Lens.Lens' GetBotChannelAssociation Prelude.Text
getBotChannelAssociation_name = Lens.lens (\GetBotChannelAssociation' {name} -> name) (\s@GetBotChannelAssociation' {} a -> s {name = a} :: GetBotChannelAssociation)

-- | The name of the Amazon Lex bot.
getBotChannelAssociation_botName :: Lens.Lens' GetBotChannelAssociation Prelude.Text
getBotChannelAssociation_botName = Lens.lens (\GetBotChannelAssociation' {botName} -> botName) (\s@GetBotChannelAssociation' {} a -> s {botName = a} :: GetBotChannelAssociation)

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
getBotChannelAssociation_botAlias :: Lens.Lens' GetBotChannelAssociation Prelude.Text
getBotChannelAssociation_botAlias = Lens.lens (\GetBotChannelAssociation' {botAlias} -> botAlias) (\s@GetBotChannelAssociation' {} a -> s {botAlias = a} :: GetBotChannelAssociation)

instance Prelude.AWSRequest GetBotChannelAssociation where
  type
    Rs GetBotChannelAssociation =
      GetBotChannelAssociationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationResponse'
            Prelude.<$> (x Prelude..?> "botAlias")
            Prelude.<*> (x Prelude..?> "createdDate")
            Prelude.<*> (x Prelude..?> "status")
            Prelude.<*> ( x Prelude..?> "botConfiguration"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "botName")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "failureReason")
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (x Prelude..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotChannelAssociation

instance Prelude.NFData GetBotChannelAssociation

instance Prelude.ToHeaders GetBotChannelAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetBotChannelAssociation where
  toPath GetBotChannelAssociation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Prelude.toBS botName,
        "/aliases/",
        Prelude.toBS botAlias,
        "/channels/",
        Prelude.toBS name
      ]

instance Prelude.ToQuery GetBotChannelAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Prelude.Maybe Prelude.Text,
    -- | The date that the association between the bot and the channel was
    -- created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the bot channel.
    --
    -- -   @CREATED@ - The channel has been created and is ready for use.
    --
    -- -   @IN_PROGRESS@ - Channel creation is in progress.
    --
    -- -   @FAILED@ - There was an error creating the channel. For information
    --     about the reason for the failure, see the @failureReason@ field.
    status :: Prelude.Maybe ChannelStatus,
    -- | Provides information that the messaging platform needs to communicate
    -- with the Amazon Lex bot.
    botConfiguration :: Prelude.Maybe (Prelude.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the Amazon Lex bot.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The name of the association between the bot and the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to create the association.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A description of the association between the bot and the channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the messaging platform.
    type' :: Prelude.Maybe ChannelType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBotChannelAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAlias', 'getBotChannelAssociationResponse_botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
--
-- 'createdDate', 'getBotChannelAssociationResponse_createdDate' - The date that the association between the bot and the channel was
-- created.
--
-- 'status', 'getBotChannelAssociationResponse_status' - The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
--
-- 'botConfiguration', 'getBotChannelAssociationResponse_botConfiguration' - Provides information that the messaging platform needs to communicate
-- with the Amazon Lex bot.
--
-- 'botName', 'getBotChannelAssociationResponse_botName' - The name of the Amazon Lex bot.
--
-- 'name', 'getBotChannelAssociationResponse_name' - The name of the association between the bot and the channel.
--
-- 'failureReason', 'getBotChannelAssociationResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
--
-- 'description', 'getBotChannelAssociationResponse_description' - A description of the association between the bot and the channel.
--
-- 'type'', 'getBotChannelAssociationResponse_type' - The type of the messaging platform.
--
-- 'httpStatus', 'getBotChannelAssociationResponse_httpStatus' - The response's http status code.
newGetBotChannelAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotChannelAssociationResponse
newGetBotChannelAssociationResponse pHttpStatus_ =
  GetBotChannelAssociationResponse'
    { botAlias =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      status = Prelude.Nothing,
      botConfiguration = Prelude.Nothing,
      botName = Prelude.Nothing,
      name = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
getBotChannelAssociationResponse_botAlias :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_botAlias = Lens.lens (\GetBotChannelAssociationResponse' {botAlias} -> botAlias) (\s@GetBotChannelAssociationResponse' {} a -> s {botAlias = a} :: GetBotChannelAssociationResponse)

-- | The date that the association between the bot and the channel was
-- created.
getBotChannelAssociationResponse_createdDate :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.UTCTime)
getBotChannelAssociationResponse_createdDate = Lens.lens (\GetBotChannelAssociationResponse' {createdDate} -> createdDate) (\s@GetBotChannelAssociationResponse' {} a -> s {createdDate = a} :: GetBotChannelAssociationResponse) Prelude.. Lens.mapping Prelude._Time

-- | The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
getBotChannelAssociationResponse_status :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe ChannelStatus)
getBotChannelAssociationResponse_status = Lens.lens (\GetBotChannelAssociationResponse' {status} -> status) (\s@GetBotChannelAssociationResponse' {} a -> s {status = a} :: GetBotChannelAssociationResponse)

-- | Provides information that the messaging platform needs to communicate
-- with the Amazon Lex bot.
getBotChannelAssociationResponse_botConfiguration :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getBotChannelAssociationResponse_botConfiguration = Lens.lens (\GetBotChannelAssociationResponse' {botConfiguration} -> botConfiguration) (\s@GetBotChannelAssociationResponse' {} a -> s {botConfiguration = a} :: GetBotChannelAssociationResponse) Prelude.. Lens.mapping (Prelude._Sensitive Prelude.. Prelude._Coerce)

-- | The name of the Amazon Lex bot.
getBotChannelAssociationResponse_botName :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_botName = Lens.lens (\GetBotChannelAssociationResponse' {botName} -> botName) (\s@GetBotChannelAssociationResponse' {} a -> s {botName = a} :: GetBotChannelAssociationResponse)

-- | The name of the association between the bot and the channel.
getBotChannelAssociationResponse_name :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_name = Lens.lens (\GetBotChannelAssociationResponse' {name} -> name) (\s@GetBotChannelAssociationResponse' {} a -> s {name = a} :: GetBotChannelAssociationResponse)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
getBotChannelAssociationResponse_failureReason :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_failureReason = Lens.lens (\GetBotChannelAssociationResponse' {failureReason} -> failureReason) (\s@GetBotChannelAssociationResponse' {} a -> s {failureReason = a} :: GetBotChannelAssociationResponse)

-- | A description of the association between the bot and the channel.
getBotChannelAssociationResponse_description :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_description = Lens.lens (\GetBotChannelAssociationResponse' {description} -> description) (\s@GetBotChannelAssociationResponse' {} a -> s {description = a} :: GetBotChannelAssociationResponse)

-- | The type of the messaging platform.
getBotChannelAssociationResponse_type :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe ChannelType)
getBotChannelAssociationResponse_type = Lens.lens (\GetBotChannelAssociationResponse' {type'} -> type') (\s@GetBotChannelAssociationResponse' {} a -> s {type' = a} :: GetBotChannelAssociationResponse)

-- | The response's http status code.
getBotChannelAssociationResponse_httpStatus :: Lens.Lens' GetBotChannelAssociationResponse Prelude.Int
getBotChannelAssociationResponse_httpStatus = Lens.lens (\GetBotChannelAssociationResponse' {httpStatus} -> httpStatus) (\s@GetBotChannelAssociationResponse' {} a -> s {httpStatus = a} :: GetBotChannelAssociationResponse)

instance
  Prelude.NFData
    GetBotChannelAssociationResponse
