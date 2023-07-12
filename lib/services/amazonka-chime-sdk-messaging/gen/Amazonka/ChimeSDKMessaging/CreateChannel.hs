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
-- Module      : Amazonka.ChimeSDKMessaging.CreateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel to which you can add users and send messages.
--
-- __Restriction__: You can\'t change a channel\'s privacy.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.ChimeSDKMessaging.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_channelId,
    createChannel_elasticChannelConfiguration,
    createChannel_memberArns,
    createChannel_metadata,
    createChannel_mode,
    createChannel_moderatorArns,
    createChannel_privacy,
    createChannel_tags,
    createChannel_appInstanceArn,
    createChannel_name,
    createChannel_clientRequestToken,
    createChannel_chimeBearer,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channelArn,
    createChannelResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | The ID of the channel in the request.
    channelId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The attributes required to configure and create an elastic channel. An
    -- elastic channel can support a maximum of 1-million users, excluding
    -- moderators.
    elasticChannelConfiguration :: Prelude.Maybe ElasticChannelConfiguration,
    -- | The ARNs of the channel members in the request.
    memberArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The metadata of the creation request. Limited to 1KB and UTF-8.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The channel mode: @UNRESTRICTED@ or @RESTRICTED@. Administrators,
    -- moderators, and channel members can add themselves and other members to
    -- unrestricted channels. Only administrators and moderators can add
    -- members to restricted channels.
    mode :: Prelude.Maybe ChannelMode,
    -- | The ARNs of the channel moderators in the request.
    moderatorArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The channel\'s privacy level: @PUBLIC@ or @PRIVATE@. Private channels
    -- aren\'t discoverable by users outside the channel. Public channels are
    -- discoverable by anyone in the @AppInstance@.
    privacy :: Prelude.Maybe ChannelPrivacy,
    -- | The tags for the creation request.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ARN of the channel request.
    appInstanceArn :: Prelude.Text,
    -- | The name of the channel.
    name :: Data.Sensitive Prelude.Text,
    -- | The client token for the request. An @Idempotency@ token.
    clientRequestToken :: Data.Sensitive Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'createChannel_channelId' - The ID of the channel in the request.
--
-- 'elasticChannelConfiguration', 'createChannel_elasticChannelConfiguration' - The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million users, excluding
-- moderators.
--
-- 'memberArns', 'createChannel_memberArns' - The ARNs of the channel members in the request.
--
-- 'metadata', 'createChannel_metadata' - The metadata of the creation request. Limited to 1KB and UTF-8.
--
-- 'mode', 'createChannel_mode' - The channel mode: @UNRESTRICTED@ or @RESTRICTED@. Administrators,
-- moderators, and channel members can add themselves and other members to
-- unrestricted channels. Only administrators and moderators can add
-- members to restricted channels.
--
-- 'moderatorArns', 'createChannel_moderatorArns' - The ARNs of the channel moderators in the request.
--
-- 'privacy', 'createChannel_privacy' - The channel\'s privacy level: @PUBLIC@ or @PRIVATE@. Private channels
-- aren\'t discoverable by users outside the channel. Public channels are
-- discoverable by anyone in the @AppInstance@.
--
-- 'tags', 'createChannel_tags' - The tags for the creation request.
--
-- 'appInstanceArn', 'createChannel_appInstanceArn' - The ARN of the channel request.
--
-- 'name', 'createChannel_name' - The name of the channel.
--
-- 'clientRequestToken', 'createChannel_clientRequestToken' - The client token for the request. An @Idempotency@ token.
--
-- 'chimeBearer', 'createChannel_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newCreateChannel ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  CreateChannel
newCreateChannel
  pAppInstanceArn_
  pName_
  pClientRequestToken_
  pChimeBearer_ =
    CreateChannel'
      { channelId = Prelude.Nothing,
        elasticChannelConfiguration = Prelude.Nothing,
        memberArns = Prelude.Nothing,
        metadata = Prelude.Nothing,
        mode = Prelude.Nothing,
        moderatorArns = Prelude.Nothing,
        privacy = Prelude.Nothing,
        tags = Prelude.Nothing,
        appInstanceArn = pAppInstanceArn_,
        name = Data._Sensitive Lens.# pName_,
        clientRequestToken =
          Data._Sensitive Lens.# pClientRequestToken_,
        chimeBearer = pChimeBearer_
      }

-- | The ID of the channel in the request.
createChannel_channelId :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_channelId = Lens.lens (\CreateChannel' {channelId} -> channelId) (\s@CreateChannel' {} a -> s {channelId = a} :: CreateChannel) Prelude.. Lens.mapping Data._Sensitive

-- | The attributes required to configure and create an elastic channel. An
-- elastic channel can support a maximum of 1-million users, excluding
-- moderators.
createChannel_elasticChannelConfiguration :: Lens.Lens' CreateChannel (Prelude.Maybe ElasticChannelConfiguration)
createChannel_elasticChannelConfiguration = Lens.lens (\CreateChannel' {elasticChannelConfiguration} -> elasticChannelConfiguration) (\s@CreateChannel' {} a -> s {elasticChannelConfiguration = a} :: CreateChannel)

-- | The ARNs of the channel members in the request.
createChannel_memberArns :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createChannel_memberArns = Lens.lens (\CreateChannel' {memberArns} -> memberArns) (\s@CreateChannel' {} a -> s {memberArns = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The metadata of the creation request. Limited to 1KB and UTF-8.
createChannel_metadata :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_metadata = Lens.lens (\CreateChannel' {metadata} -> metadata) (\s@CreateChannel' {} a -> s {metadata = a} :: CreateChannel) Prelude.. Lens.mapping Data._Sensitive

-- | The channel mode: @UNRESTRICTED@ or @RESTRICTED@. Administrators,
-- moderators, and channel members can add themselves and other members to
-- unrestricted channels. Only administrators and moderators can add
-- members to restricted channels.
createChannel_mode :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelMode)
createChannel_mode = Lens.lens (\CreateChannel' {mode} -> mode) (\s@CreateChannel' {} a -> s {mode = a} :: CreateChannel)

-- | The ARNs of the channel moderators in the request.
createChannel_moderatorArns :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createChannel_moderatorArns = Lens.lens (\CreateChannel' {moderatorArns} -> moderatorArns) (\s@CreateChannel' {} a -> s {moderatorArns = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The channel\'s privacy level: @PUBLIC@ or @PRIVATE@. Private channels
-- aren\'t discoverable by users outside the channel. Public channels are
-- discoverable by anyone in the @AppInstance@.
createChannel_privacy :: Lens.Lens' CreateChannel (Prelude.Maybe ChannelPrivacy)
createChannel_privacy = Lens.lens (\CreateChannel' {privacy} -> privacy) (\s@CreateChannel' {} a -> s {privacy = a} :: CreateChannel)

-- | The tags for the creation request.
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.NonEmpty Tag))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the channel request.
createChannel_appInstanceArn :: Lens.Lens' CreateChannel Prelude.Text
createChannel_appInstanceArn = Lens.lens (\CreateChannel' {appInstanceArn} -> appInstanceArn) (\s@CreateChannel' {} a -> s {appInstanceArn = a} :: CreateChannel)

-- | The name of the channel.
createChannel_name :: Lens.Lens' CreateChannel Prelude.Text
createChannel_name = Lens.lens (\CreateChannel' {name} -> name) (\s@CreateChannel' {} a -> s {name = a} :: CreateChannel) Prelude.. Data._Sensitive

-- | The client token for the request. An @Idempotency@ token.
createChannel_clientRequestToken :: Lens.Lens' CreateChannel Prelude.Text
createChannel_clientRequestToken = Lens.lens (\CreateChannel' {clientRequestToken} -> clientRequestToken) (\s@CreateChannel' {} a -> s {clientRequestToken = a} :: CreateChannel) Prelude.. Data._Sensitive

-- | The @AppInstanceUserArn@ of the user that makes the API call.
createChannel_chimeBearer :: Lens.Lens' CreateChannel Prelude.Text
createChannel_chimeBearer = Lens.lens (\CreateChannel' {chimeBearer} -> chimeBearer) (\s@CreateChannel' {} a -> s {chimeBearer = a} :: CreateChannel)

instance Core.AWSRequest CreateChannel where
  type
    AWSResponse CreateChannel =
      CreateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` elasticChannelConfiguration
      `Prelude.hashWithSalt` memberArns
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` moderatorArns
      `Prelude.hashWithSalt` privacy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` appInstanceArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf elasticChannelConfiguration
      `Prelude.seq` Prelude.rnf memberArns
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf moderatorArns
      `Prelude.seq` Prelude.rnf privacy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders CreateChannel where
  toHeaders CreateChannel' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChannelId" Data..=) Prelude.<$> channelId,
            ("ElasticChannelConfiguration" Data..=)
              Prelude.<$> elasticChannelConfiguration,
            ("MemberArns" Data..=) Prelude.<$> memberArns,
            ("Metadata" Data..=) Prelude.<$> metadata,
            ("Mode" Data..=) Prelude.<$> mode,
            ("ModeratorArns" Data..=) Prelude.<$> moderatorArns,
            ("Privacy" Data..=) Prelude.<$> privacy,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("AppInstanceArn" Data..= appInstanceArn),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateChannel where
  toPath = Prelude.const "/channels"

instance Data.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'createChannelResponse_channelArn' - The ARN of the channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channelArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
createChannelResponse_channelArn :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_channelArn = Lens.lens (\CreateChannelResponse' {channelArn} -> channelArn) (\s@CreateChannelResponse' {} a -> s {channelArn = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf httpStatus
