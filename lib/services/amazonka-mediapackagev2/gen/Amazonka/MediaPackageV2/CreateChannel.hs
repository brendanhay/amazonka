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
-- Module      : Amazonka.MediaPackageV2.CreateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a channel to start receiving content streams. The channel
-- represents the input to MediaPackage for incoming live content from an
-- encoder such as AWS Elemental MediaLive. The channel receives content,
-- and after packaging it, outputs it through an origin endpoint to
-- downstream devices (such as video players or CDNs) that request the
-- content. You can create only one channel with each request. We recommend
-- that you spread out channels between channel groups, such as putting
-- redundant channels in the same AWS Region in different channel groups.
module Amazonka.MediaPackageV2.CreateChannel
  ( -- * Creating a Request
    CreateChannel (..),
    newCreateChannel,

    -- * Request Lenses
    createChannel_clientToken,
    createChannel_description,
    createChannel_tags,
    createChannel_channelGroupName,
    createChannel_channelName,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_description,
    createChannelResponse_ingestEndpoints,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,
    createChannelResponse_arn,
    createChannelResponse_channelName,
    createChannelResponse_channelGroupName,
    createChannelResponse_createdAt,
    createChannelResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannel' smart constructor.
data CreateChannel = CreateChannel'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Enter any descriptive text that helps you to identify the channel.
    description :: Prelude.Maybe Prelude.Text,
    -- | A comma-separated list of tag key:value pairs that you define. For
    -- example:
    --
    -- @\"Key1\": \"Value1\",@
    --
    -- @\"Key2\": \"Value2\"@
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group. You can\'t change the name after you create the
    -- channel.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createChannel_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createChannel_description' - Enter any descriptive text that helps you to identify the channel.
--
-- 'tags', 'createChannel_tags' - A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
--
-- 'channelGroupName', 'createChannel_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'createChannel_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group. You can\'t change the name after you create the
-- channel.
newCreateChannel ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  CreateChannel
newCreateChannel pChannelGroupName_ pChannelName_ =
  CreateChannel'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      channelGroupName = pChannelGroupName_,
      channelName = pChannelName_
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createChannel_clientToken :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_clientToken = Lens.lens (\CreateChannel' {clientToken} -> clientToken) (\s@CreateChannel' {} a -> s {clientToken = a} :: CreateChannel)

-- | Enter any descriptive text that helps you to identify the channel.
createChannel_description :: Lens.Lens' CreateChannel (Prelude.Maybe Prelude.Text)
createChannel_description = Lens.lens (\CreateChannel' {description} -> description) (\s@CreateChannel' {} a -> s {description = a} :: CreateChannel)

-- | A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
createChannel_tags :: Lens.Lens' CreateChannel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel_tags = Lens.lens (\CreateChannel' {tags} -> tags) (\s@CreateChannel' {} a -> s {tags = a} :: CreateChannel) Prelude.. Lens.mapping Lens.coerced

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
createChannel_channelGroupName :: Lens.Lens' CreateChannel Prelude.Text
createChannel_channelGroupName = Lens.lens (\CreateChannel' {channelGroupName} -> channelGroupName) (\s@CreateChannel' {} a -> s {channelGroupName = a} :: CreateChannel)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group. You can\'t change the name after you create the
-- channel.
createChannel_channelName :: Lens.Lens' CreateChannel Prelude.Text
createChannel_channelName = Lens.lens (\CreateChannel' {channelName} -> channelName) (\s@CreateChannel' {} a -> s {channelName = a} :: CreateChannel)

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
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> ( x
                            Data..?> "IngestEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "ChannelName")
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable CreateChannel where
  hashWithSalt _salt CreateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData CreateChannel where
  rnf CreateChannel' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders CreateChannel where
  toHeaders CreateChannel' {..} =
    Prelude.mconcat
      [ "x-amzn-client-token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ChannelName" Data..= channelName)
          ]
      )

instance Data.ToPath CreateChannel where
  toPath CreateChannel' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel"
      ]

instance Data.ToQuery CreateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { -- | The description for your channel.
    description :: Prelude.Maybe Prelude.Text,
    ingestEndpoints :: Prelude.Maybe [IngestEndpoint],
    -- | The comma-separated list of tag key:value pairs assigned to the channel.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The date and time the channel was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel was modified.
    modifiedAt :: Data.POSIX
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
-- 'description', 'createChannelResponse_description' - The description for your channel.
--
-- 'ingestEndpoints', 'createChannelResponse_ingestEndpoints' - Undocumented member.
--
-- 'tags', 'createChannelResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createChannelResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelName', 'createChannelResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'channelGroupName', 'createChannelResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'createdAt', 'createChannelResponse_createdAt' - The date and time the channel was created.
--
-- 'modifiedAt', 'createChannelResponse_modifiedAt' - The date and time the channel was modified.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  CreateChannelResponse
newCreateChannelResponse
  pHttpStatus_
  pArn_
  pChannelName_
  pChannelGroupName_
  pCreatedAt_
  pModifiedAt_ =
    CreateChannelResponse'
      { description =
          Prelude.Nothing,
        ingestEndpoints = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        channelName = pChannelName_,
        channelGroupName = pChannelGroupName_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | The description for your channel.
createChannelResponse_description :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Prelude.Text)
createChannelResponse_description = Lens.lens (\CreateChannelResponse' {description} -> description) (\s@CreateChannelResponse' {} a -> s {description = a} :: CreateChannelResponse)

-- | Undocumented member.
createChannelResponse_ingestEndpoints :: Lens.Lens' CreateChannelResponse (Prelude.Maybe [IngestEndpoint])
createChannelResponse_ingestEndpoints = Lens.lens (\CreateChannelResponse' {ingestEndpoints} -> ingestEndpoints) (\s@CreateChannelResponse' {} a -> s {ingestEndpoints = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The comma-separated list of tag key:value pairs assigned to the channel.
createChannelResponse_tags :: Lens.Lens' CreateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannelResponse_tags = Lens.lens (\CreateChannelResponse' {tags} -> tags) (\s@CreateChannelResponse' {} a -> s {tags = a} :: CreateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
createChannelResponse_arn :: Lens.Lens' CreateChannelResponse Prelude.Text
createChannelResponse_arn = Lens.lens (\CreateChannelResponse' {arn} -> arn) (\s@CreateChannelResponse' {} a -> s {arn = a} :: CreateChannelResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
createChannelResponse_channelName :: Lens.Lens' CreateChannelResponse Prelude.Text
createChannelResponse_channelName = Lens.lens (\CreateChannelResponse' {channelName} -> channelName) (\s@CreateChannelResponse' {} a -> s {channelName = a} :: CreateChannelResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
createChannelResponse_channelGroupName :: Lens.Lens' CreateChannelResponse Prelude.Text
createChannelResponse_channelGroupName = Lens.lens (\CreateChannelResponse' {channelGroupName} -> channelGroupName) (\s@CreateChannelResponse' {} a -> s {channelGroupName = a} :: CreateChannelResponse)

-- | The date and time the channel was created.
createChannelResponse_createdAt :: Lens.Lens' CreateChannelResponse Prelude.UTCTime
createChannelResponse_createdAt = Lens.lens (\CreateChannelResponse' {createdAt} -> createdAt) (\s@CreateChannelResponse' {} a -> s {createdAt = a} :: CreateChannelResponse) Prelude.. Data._Time

-- | The date and time the channel was modified.
createChannelResponse_modifiedAt :: Lens.Lens' CreateChannelResponse Prelude.UTCTime
createChannelResponse_modifiedAt = Lens.lens (\CreateChannelResponse' {modifiedAt} -> modifiedAt) (\s@CreateChannelResponse' {} a -> s {modifiedAt = a} :: CreateChannelResponse) Prelude.. Data._Time

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ingestEndpoints
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
