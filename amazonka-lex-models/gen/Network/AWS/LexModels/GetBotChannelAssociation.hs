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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotChannelAssociation' smart constructor.
data GetBotChannelAssociation = GetBotChannelAssociation'
  { -- | The name of the association between the bot and the channel. The name is
    -- case sensitive.
    name :: Core.Text,
    -- | The name of the Amazon Lex bot.
    botName :: Core.Text,
    -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'botName'
  Core.Text ->
  -- | 'botAlias'
  Core.Text ->
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
getBotChannelAssociation_name :: Lens.Lens' GetBotChannelAssociation Core.Text
getBotChannelAssociation_name = Lens.lens (\GetBotChannelAssociation' {name} -> name) (\s@GetBotChannelAssociation' {} a -> s {name = a} :: GetBotChannelAssociation)

-- | The name of the Amazon Lex bot.
getBotChannelAssociation_botName :: Lens.Lens' GetBotChannelAssociation Core.Text
getBotChannelAssociation_botName = Lens.lens (\GetBotChannelAssociation' {botName} -> botName) (\s@GetBotChannelAssociation' {} a -> s {botName = a} :: GetBotChannelAssociation)

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
getBotChannelAssociation_botAlias :: Lens.Lens' GetBotChannelAssociation Core.Text
getBotChannelAssociation_botAlias = Lens.lens (\GetBotChannelAssociation' {botAlias} -> botAlias) (\s@GetBotChannelAssociation' {} a -> s {botAlias = a} :: GetBotChannelAssociation)

instance Core.AWSRequest GetBotChannelAssociation where
  type
    AWSResponse GetBotChannelAssociation =
      GetBotChannelAssociationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotChannelAssociationResponse'
            Core.<$> (x Core..?> "botAlias")
            Core.<*> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "botConfiguration" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "botName")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "failureReason")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "type")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBotChannelAssociation

instance Core.NFData GetBotChannelAssociation

instance Core.ToHeaders GetBotChannelAssociation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBotChannelAssociation where
  toPath GetBotChannelAssociation' {..} =
    Core.mconcat
      [ "/bots/",
        Core.toBS botName,
        "/aliases/",
        Core.toBS botAlias,
        "/channels/",
        Core.toBS name
      ]

instance Core.ToQuery GetBotChannelAssociation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetBotChannelAssociationResponse' smart constructor.
data GetBotChannelAssociationResponse = GetBotChannelAssociationResponse'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Core.Maybe Core.Text,
    -- | The date that the association between the bot and the channel was
    -- created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The status of the bot channel.
    --
    -- -   @CREATED@ - The channel has been created and is ready for use.
    --
    -- -   @IN_PROGRESS@ - Channel creation is in progress.
    --
    -- -   @FAILED@ - There was an error creating the channel. For information
    --     about the reason for the failure, see the @failureReason@ field.
    status :: Core.Maybe ChannelStatus,
    -- | Provides information that the messaging platform needs to communicate
    -- with the Amazon Lex bot.
    botConfiguration :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | The name of the Amazon Lex bot.
    botName :: Core.Maybe Core.Text,
    -- | The name of the association between the bot and the channel.
    name :: Core.Maybe Core.Text,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to create the association.
    failureReason :: Core.Maybe Core.Text,
    -- | A description of the association between the bot and the channel.
    description :: Core.Maybe Core.Text,
    -- | The type of the messaging platform.
    type' :: Core.Maybe ChannelType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  GetBotChannelAssociationResponse
newGetBotChannelAssociationResponse pHttpStatus_ =
  GetBotChannelAssociationResponse'
    { botAlias =
        Core.Nothing,
      createdDate = Core.Nothing,
      status = Core.Nothing,
      botConfiguration = Core.Nothing,
      botName = Core.Nothing,
      name = Core.Nothing,
      failureReason = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
getBotChannelAssociationResponse_botAlias :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.Text)
getBotChannelAssociationResponse_botAlias = Lens.lens (\GetBotChannelAssociationResponse' {botAlias} -> botAlias) (\s@GetBotChannelAssociationResponse' {} a -> s {botAlias = a} :: GetBotChannelAssociationResponse)

-- | The date that the association between the bot and the channel was
-- created.
getBotChannelAssociationResponse_createdDate :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.UTCTime)
getBotChannelAssociationResponse_createdDate = Lens.lens (\GetBotChannelAssociationResponse' {createdDate} -> createdDate) (\s@GetBotChannelAssociationResponse' {} a -> s {createdDate = a} :: GetBotChannelAssociationResponse) Core.. Lens.mapping Core._Time

-- | The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
getBotChannelAssociationResponse_status :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe ChannelStatus)
getBotChannelAssociationResponse_status = Lens.lens (\GetBotChannelAssociationResponse' {status} -> status) (\s@GetBotChannelAssociationResponse' {} a -> s {status = a} :: GetBotChannelAssociationResponse)

-- | Provides information that the messaging platform needs to communicate
-- with the Amazon Lex bot.
getBotChannelAssociationResponse_botConfiguration :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
getBotChannelAssociationResponse_botConfiguration = Lens.lens (\GetBotChannelAssociationResponse' {botConfiguration} -> botConfiguration) (\s@GetBotChannelAssociationResponse' {} a -> s {botConfiguration = a} :: GetBotChannelAssociationResponse) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The name of the Amazon Lex bot.
getBotChannelAssociationResponse_botName :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.Text)
getBotChannelAssociationResponse_botName = Lens.lens (\GetBotChannelAssociationResponse' {botName} -> botName) (\s@GetBotChannelAssociationResponse' {} a -> s {botName = a} :: GetBotChannelAssociationResponse)

-- | The name of the association between the bot and the channel.
getBotChannelAssociationResponse_name :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.Text)
getBotChannelAssociationResponse_name = Lens.lens (\GetBotChannelAssociationResponse' {name} -> name) (\s@GetBotChannelAssociationResponse' {} a -> s {name = a} :: GetBotChannelAssociationResponse)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
getBotChannelAssociationResponse_failureReason :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.Text)
getBotChannelAssociationResponse_failureReason = Lens.lens (\GetBotChannelAssociationResponse' {failureReason} -> failureReason) (\s@GetBotChannelAssociationResponse' {} a -> s {failureReason = a} :: GetBotChannelAssociationResponse)

-- | A description of the association between the bot and the channel.
getBotChannelAssociationResponse_description :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe Core.Text)
getBotChannelAssociationResponse_description = Lens.lens (\GetBotChannelAssociationResponse' {description} -> description) (\s@GetBotChannelAssociationResponse' {} a -> s {description = a} :: GetBotChannelAssociationResponse)

-- | The type of the messaging platform.
getBotChannelAssociationResponse_type :: Lens.Lens' GetBotChannelAssociationResponse (Core.Maybe ChannelType)
getBotChannelAssociationResponse_type = Lens.lens (\GetBotChannelAssociationResponse' {type'} -> type') (\s@GetBotChannelAssociationResponse' {} a -> s {type' = a} :: GetBotChannelAssociationResponse)

-- | The response's http status code.
getBotChannelAssociationResponse_httpStatus :: Lens.Lens' GetBotChannelAssociationResponse Core.Int
getBotChannelAssociationResponse_httpStatus = Lens.lens (\GetBotChannelAssociationResponse' {httpStatus} -> httpStatus) (\s@GetBotChannelAssociationResponse' {} a -> s {httpStatus = a} :: GetBotChannelAssociationResponse)

instance Core.NFData GetBotChannelAssociationResponse
