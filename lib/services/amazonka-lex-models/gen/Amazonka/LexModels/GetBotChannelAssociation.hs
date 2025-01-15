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
-- Module      : Amazonka.LexModels.GetBotChannelAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the association between an Amazon Lex bot and
-- a messaging platform.
--
-- This operation requires permissions for the
-- @lex:GetBotChannelAssociation@ action.
module Amazonka.LexModels.GetBotChannelAssociation
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
    getBotChannelAssociationResponse_botConfiguration,
    getBotChannelAssociationResponse_botName,
    getBotChannelAssociationResponse_createdDate,
    getBotChannelAssociationResponse_description,
    getBotChannelAssociationResponse_failureReason,
    getBotChannelAssociationResponse_name,
    getBotChannelAssociationResponse_status,
    getBotChannelAssociationResponse_type,
    getBotChannelAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetBotChannelAssociation where
  type
    AWSResponse GetBotChannelAssociation =
      GetBotChannelAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationResponse'
            Prelude.<$> (x Data..?> "botAlias")
            Prelude.<*> ( x
                            Data..?> "botConfiguration"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "failureReason")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotChannelAssociation where
  hashWithSalt _salt GetBotChannelAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` botAlias

instance Prelude.NFData GetBotChannelAssociation where
  rnf GetBotChannelAssociation' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf botName `Prelude.seq`
        Prelude.rnf botAlias

instance Data.ToHeaders GetBotChannelAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBotChannelAssociation where
  toPath GetBotChannelAssociation' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botName,
        "/aliases/",
        Data.toBS botAlias,
        "/channels/",
        Data.toBS name
      ]

instance Data.ToQuery GetBotChannelAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Prelude.Maybe Prelude.Text,
    -- | Provides information that the messaging platform needs to communicate
    -- with the Amazon Lex bot.
    botConfiguration :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name of the Amazon Lex bot.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The date that the association between the bot and the channel was
    -- created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the association between the bot and the channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to create the association.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the association between the bot and the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the bot channel.
    --
    -- -   @CREATED@ - The channel has been created and is ready for use.
    --
    -- -   @IN_PROGRESS@ - Channel creation is in progress.
    --
    -- -   @FAILED@ - There was an error creating the channel. For information
    --     about the reason for the failure, see the @failureReason@ field.
    status :: Prelude.Maybe ChannelStatus,
    -- | The type of the messaging platform.
    type' :: Prelude.Maybe ChannelType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'botConfiguration', 'getBotChannelAssociationResponse_botConfiguration' - Provides information that the messaging platform needs to communicate
-- with the Amazon Lex bot.
--
-- 'botName', 'getBotChannelAssociationResponse_botName' - The name of the Amazon Lex bot.
--
-- 'createdDate', 'getBotChannelAssociationResponse_createdDate' - The date that the association between the bot and the channel was
-- created.
--
-- 'description', 'getBotChannelAssociationResponse_description' - A description of the association between the bot and the channel.
--
-- 'failureReason', 'getBotChannelAssociationResponse_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
--
-- 'name', 'getBotChannelAssociationResponse_name' - The name of the association between the bot and the channel.
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
      botConfiguration = Prelude.Nothing,
      botName = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
getBotChannelAssociationResponse_botAlias :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_botAlias = Lens.lens (\GetBotChannelAssociationResponse' {botAlias} -> botAlias) (\s@GetBotChannelAssociationResponse' {} a -> s {botAlias = a} :: GetBotChannelAssociationResponse)

-- | Provides information that the messaging platform needs to communicate
-- with the Amazon Lex bot.
getBotChannelAssociationResponse_botConfiguration :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getBotChannelAssociationResponse_botConfiguration = Lens.lens (\GetBotChannelAssociationResponse' {botConfiguration} -> botConfiguration) (\s@GetBotChannelAssociationResponse' {} a -> s {botConfiguration = a} :: GetBotChannelAssociationResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name of the Amazon Lex bot.
getBotChannelAssociationResponse_botName :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_botName = Lens.lens (\GetBotChannelAssociationResponse' {botName} -> botName) (\s@GetBotChannelAssociationResponse' {} a -> s {botName = a} :: GetBotChannelAssociationResponse)

-- | The date that the association between the bot and the channel was
-- created.
getBotChannelAssociationResponse_createdDate :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.UTCTime)
getBotChannelAssociationResponse_createdDate = Lens.lens (\GetBotChannelAssociationResponse' {createdDate} -> createdDate) (\s@GetBotChannelAssociationResponse' {} a -> s {createdDate = a} :: GetBotChannelAssociationResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the association between the bot and the channel.
getBotChannelAssociationResponse_description :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_description = Lens.lens (\GetBotChannelAssociationResponse' {description} -> description) (\s@GetBotChannelAssociationResponse' {} a -> s {description = a} :: GetBotChannelAssociationResponse)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
getBotChannelAssociationResponse_failureReason :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_failureReason = Lens.lens (\GetBotChannelAssociationResponse' {failureReason} -> failureReason) (\s@GetBotChannelAssociationResponse' {} a -> s {failureReason = a} :: GetBotChannelAssociationResponse)

-- | The name of the association between the bot and the channel.
getBotChannelAssociationResponse_name :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe Prelude.Text)
getBotChannelAssociationResponse_name = Lens.lens (\GetBotChannelAssociationResponse' {name} -> name) (\s@GetBotChannelAssociationResponse' {} a -> s {name = a} :: GetBotChannelAssociationResponse)

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

-- | The type of the messaging platform.
getBotChannelAssociationResponse_type :: Lens.Lens' GetBotChannelAssociationResponse (Prelude.Maybe ChannelType)
getBotChannelAssociationResponse_type = Lens.lens (\GetBotChannelAssociationResponse' {type'} -> type') (\s@GetBotChannelAssociationResponse' {} a -> s {type' = a} :: GetBotChannelAssociationResponse)

-- | The response's http status code.
getBotChannelAssociationResponse_httpStatus :: Lens.Lens' GetBotChannelAssociationResponse Prelude.Int
getBotChannelAssociationResponse_httpStatus = Lens.lens (\GetBotChannelAssociationResponse' {httpStatus} -> httpStatus) (\s@GetBotChannelAssociationResponse' {} a -> s {httpStatus = a} :: GetBotChannelAssociationResponse)

instance
  Prelude.NFData
    GetBotChannelAssociationResponse
  where
  rnf GetBotChannelAssociationResponse' {..} =
    Prelude.rnf botAlias `Prelude.seq`
      Prelude.rnf botConfiguration `Prelude.seq`
        Prelude.rnf botName `Prelude.seq`
          Prelude.rnf createdDate `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf failureReason `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf type' `Prelude.seq`
                      Prelude.rnf httpStatus
