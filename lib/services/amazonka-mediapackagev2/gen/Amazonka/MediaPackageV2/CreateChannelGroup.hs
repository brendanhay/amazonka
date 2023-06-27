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
-- Module      : Amazonka.MediaPackageV2.CreateChannelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a channel group to group your channels and origin endpoints. A
-- channel group is the top-level resource that consists of channels and
-- origin endpoints that are associated with it and that provides
-- predictable URLs for stream delivery. All channels and origin endpoints
-- within the channel group are guaranteed to share the DNS. You can create
-- only one channel group with each request.
module Amazonka.MediaPackageV2.CreateChannelGroup
  ( -- * Creating a Request
    CreateChannelGroup (..),
    newCreateChannelGroup,

    -- * Request Lenses
    createChannelGroup_clientToken,
    createChannelGroup_description,
    createChannelGroup_tags,
    createChannelGroup_channelGroupName,

    -- * Destructuring the Response
    CreateChannelGroupResponse (..),
    newCreateChannelGroupResponse,

    -- * Response Lenses
    createChannelGroupResponse_description,
    createChannelGroupResponse_tags,
    createChannelGroupResponse_httpStatus,
    createChannelGroupResponse_channelGroupName,
    createChannelGroupResponse_arn,
    createChannelGroupResponse_egressDomain,
    createChannelGroupResponse_createdAt,
    createChannelGroupResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChannelGroup' smart constructor.
data CreateChannelGroup = CreateChannelGroup'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Enter any descriptive text that helps you to identify the channel group.
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
    -- the AWS Region. You can\'t use spaces in the name. You can\'t change the
    -- name after you create the channel group.
    channelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createChannelGroup_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createChannelGroup_description' - Enter any descriptive text that helps you to identify the channel group.
--
-- 'tags', 'createChannelGroup_tags' - A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
--
-- 'channelGroupName', 'createChannelGroup_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region. You can\'t use spaces in the name. You can\'t change the
-- name after you create the channel group.
newCreateChannelGroup ::
  -- | 'channelGroupName'
  Prelude.Text ->
  CreateChannelGroup
newCreateChannelGroup pChannelGroupName_ =
  CreateChannelGroup'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      channelGroupName = pChannelGroupName_
    }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createChannelGroup_clientToken :: Lens.Lens' CreateChannelGroup (Prelude.Maybe Prelude.Text)
createChannelGroup_clientToken = Lens.lens (\CreateChannelGroup' {clientToken} -> clientToken) (\s@CreateChannelGroup' {} a -> s {clientToken = a} :: CreateChannelGroup)

-- | Enter any descriptive text that helps you to identify the channel group.
createChannelGroup_description :: Lens.Lens' CreateChannelGroup (Prelude.Maybe Prelude.Text)
createChannelGroup_description = Lens.lens (\CreateChannelGroup' {description} -> description) (\s@CreateChannelGroup' {} a -> s {description = a} :: CreateChannelGroup)

-- | A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
createChannelGroup_tags :: Lens.Lens' CreateChannelGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannelGroup_tags = Lens.lens (\CreateChannelGroup' {tags} -> tags) (\s@CreateChannelGroup' {} a -> s {tags = a} :: CreateChannelGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region. You can\'t use spaces in the name. You can\'t change the
-- name after you create the channel group.
createChannelGroup_channelGroupName :: Lens.Lens' CreateChannelGroup Prelude.Text
createChannelGroup_channelGroupName = Lens.lens (\CreateChannelGroup' {channelGroupName} -> channelGroupName) (\s@CreateChannelGroup' {} a -> s {channelGroupName = a} :: CreateChannelGroup)

instance Core.AWSRequest CreateChannelGroup where
  type
    AWSResponse CreateChannelGroup =
      CreateChannelGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelGroupResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "EgressDomain")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable CreateChannelGroup where
  hashWithSalt _salt CreateChannelGroup' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelGroupName

instance Prelude.NFData CreateChannelGroup where
  rnf CreateChannelGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelGroupName

instance Data.ToHeaders CreateChannelGroup where
  toHeaders CreateChannelGroup' {..} =
    Prelude.mconcat
      [ "x-amzn-client-token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateChannelGroup where
  toJSON CreateChannelGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ChannelGroupName" Data..= channelGroupName)
          ]
      )

instance Data.ToPath CreateChannelGroup where
  toPath = Prelude.const "/channelGroup"

instance Data.ToQuery CreateChannelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChannelGroupResponse' smart constructor.
data CreateChannelGroupResponse = CreateChannelGroupResponse'
  { -- | The description for your channel group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The comma-separated list of tag key:value pairs assigned to the channel
    -- group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The output domain where the source stream should be sent. Integrate the
    -- egress domain with a downstream CDN (such as Amazon CloudFront) or
    -- playback device.
    egressDomain :: Prelude.Text,
    -- | The date and time the channel group was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel group was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createChannelGroupResponse_description' - The description for your channel group.
--
-- 'tags', 'createChannelGroupResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel
-- group.
--
-- 'httpStatus', 'createChannelGroupResponse_httpStatus' - The response's http status code.
--
-- 'channelGroupName', 'createChannelGroupResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'arn', 'createChannelGroupResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'egressDomain', 'createChannelGroupResponse_egressDomain' - The output domain where the source stream should be sent. Integrate the
-- egress domain with a downstream CDN (such as Amazon CloudFront) or
-- playback device.
--
-- 'createdAt', 'createChannelGroupResponse_createdAt' - The date and time the channel group was created.
--
-- 'modifiedAt', 'createChannelGroupResponse_modifiedAt' - The date and time the channel group was modified.
newCreateChannelGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'egressDomain'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  CreateChannelGroupResponse
newCreateChannelGroupResponse
  pHttpStatus_
  pChannelGroupName_
  pArn_
  pEgressDomain_
  pCreatedAt_
  pModifiedAt_ =
    CreateChannelGroupResponse'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        channelGroupName = pChannelGroupName_,
        arn = pArn_,
        egressDomain = pEgressDomain_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | The description for your channel group.
createChannelGroupResponse_description :: Lens.Lens' CreateChannelGroupResponse (Prelude.Maybe Prelude.Text)
createChannelGroupResponse_description = Lens.lens (\CreateChannelGroupResponse' {description} -> description) (\s@CreateChannelGroupResponse' {} a -> s {description = a} :: CreateChannelGroupResponse)

-- | The comma-separated list of tag key:value pairs assigned to the channel
-- group.
createChannelGroupResponse_tags :: Lens.Lens' CreateChannelGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannelGroupResponse_tags = Lens.lens (\CreateChannelGroupResponse' {tags} -> tags) (\s@CreateChannelGroupResponse' {} a -> s {tags = a} :: CreateChannelGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createChannelGroupResponse_httpStatus :: Lens.Lens' CreateChannelGroupResponse Prelude.Int
createChannelGroupResponse_httpStatus = Lens.lens (\CreateChannelGroupResponse' {httpStatus} -> httpStatus) (\s@CreateChannelGroupResponse' {} a -> s {httpStatus = a} :: CreateChannelGroupResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
createChannelGroupResponse_channelGroupName :: Lens.Lens' CreateChannelGroupResponse Prelude.Text
createChannelGroupResponse_channelGroupName = Lens.lens (\CreateChannelGroupResponse' {channelGroupName} -> channelGroupName) (\s@CreateChannelGroupResponse' {} a -> s {channelGroupName = a} :: CreateChannelGroupResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
createChannelGroupResponse_arn :: Lens.Lens' CreateChannelGroupResponse Prelude.Text
createChannelGroupResponse_arn = Lens.lens (\CreateChannelGroupResponse' {arn} -> arn) (\s@CreateChannelGroupResponse' {} a -> s {arn = a} :: CreateChannelGroupResponse)

-- | The output domain where the source stream should be sent. Integrate the
-- egress domain with a downstream CDN (such as Amazon CloudFront) or
-- playback device.
createChannelGroupResponse_egressDomain :: Lens.Lens' CreateChannelGroupResponse Prelude.Text
createChannelGroupResponse_egressDomain = Lens.lens (\CreateChannelGroupResponse' {egressDomain} -> egressDomain) (\s@CreateChannelGroupResponse' {} a -> s {egressDomain = a} :: CreateChannelGroupResponse)

-- | The date and time the channel group was created.
createChannelGroupResponse_createdAt :: Lens.Lens' CreateChannelGroupResponse Prelude.UTCTime
createChannelGroupResponse_createdAt = Lens.lens (\CreateChannelGroupResponse' {createdAt} -> createdAt) (\s@CreateChannelGroupResponse' {} a -> s {createdAt = a} :: CreateChannelGroupResponse) Prelude.. Data._Time

-- | The date and time the channel group was modified.
createChannelGroupResponse_modifiedAt :: Lens.Lens' CreateChannelGroupResponse Prelude.UTCTime
createChannelGroupResponse_modifiedAt = Lens.lens (\CreateChannelGroupResponse' {modifiedAt} -> modifiedAt) (\s@CreateChannelGroupResponse' {} a -> s {modifiedAt = a} :: CreateChannelGroupResponse) Prelude.. Data._Time

instance Prelude.NFData CreateChannelGroupResponse where
  rnf CreateChannelGroupResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf egressDomain
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
