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
-- Module      : Amazonka.MediaPackageV2.GetChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified channel that\'s configured in AWS Elemental
-- MediaPackage, including the origin endpoints that are associated with
-- it.
module Amazonka.MediaPackageV2.GetChannel
  ( -- * Creating a Request
    GetChannel (..),
    newGetChannel,

    -- * Request Lenses
    getChannel_channelGroupName,
    getChannel_channelName,

    -- * Destructuring the Response
    GetChannelResponse (..),
    newGetChannelResponse,

    -- * Response Lenses
    getChannelResponse_description,
    getChannelResponse_ingestEndpoints,
    getChannelResponse_tags,
    getChannelResponse_httpStatus,
    getChannelResponse_arn,
    getChannelResponse_channelName,
    getChannelResponse_channelGroupName,
    getChannelResponse_createdAt,
    getChannelResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannel' smart constructor.
data GetChannel = GetChannel'
  { -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'getChannel_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getChannel_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newGetChannel ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  GetChannel
newGetChannel pChannelGroupName_ pChannelName_ =
  GetChannel'
    { channelGroupName = pChannelGroupName_,
      channelName = pChannelName_
    }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannel_channelGroupName :: Lens.Lens' GetChannel Prelude.Text
getChannel_channelGroupName = Lens.lens (\GetChannel' {channelGroupName} -> channelGroupName) (\s@GetChannel' {} a -> s {channelGroupName = a} :: GetChannel)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getChannel_channelName :: Lens.Lens' GetChannel Prelude.Text
getChannel_channelName = Lens.lens (\GetChannel' {channelName} -> channelName) (\s@GetChannel' {} a -> s {channelName = a} :: GetChannel)

instance Core.AWSRequest GetChannel where
  type AWSResponse GetChannel = GetChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelResponse'
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

instance Prelude.Hashable GetChannel where
  hashWithSalt _salt GetChannel' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData GetChannel where
  rnf GetChannel' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders GetChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChannel where
  toPath GetChannel' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/"
      ]

instance Data.ToQuery GetChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelResponse' smart constructor.
data GetChannelResponse = GetChannelResponse'
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
-- Create a value of 'GetChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getChannelResponse_description' - The description for your channel.
--
-- 'ingestEndpoints', 'getChannelResponse_ingestEndpoints' - Undocumented member.
--
-- 'tags', 'getChannelResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel.
--
-- 'httpStatus', 'getChannelResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getChannelResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelName', 'getChannelResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'channelGroupName', 'getChannelResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'createdAt', 'getChannelResponse_createdAt' - The date and time the channel was created.
--
-- 'modifiedAt', 'getChannelResponse_modifiedAt' - The date and time the channel was modified.
newGetChannelResponse ::
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
  GetChannelResponse
newGetChannelResponse
  pHttpStatus_
  pArn_
  pChannelName_
  pChannelGroupName_
  pCreatedAt_
  pModifiedAt_ =
    GetChannelResponse'
      { description = Prelude.Nothing,
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
getChannelResponse_description :: Lens.Lens' GetChannelResponse (Prelude.Maybe Prelude.Text)
getChannelResponse_description = Lens.lens (\GetChannelResponse' {description} -> description) (\s@GetChannelResponse' {} a -> s {description = a} :: GetChannelResponse)

-- | Undocumented member.
getChannelResponse_ingestEndpoints :: Lens.Lens' GetChannelResponse (Prelude.Maybe [IngestEndpoint])
getChannelResponse_ingestEndpoints = Lens.lens (\GetChannelResponse' {ingestEndpoints} -> ingestEndpoints) (\s@GetChannelResponse' {} a -> s {ingestEndpoints = a} :: GetChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The comma-separated list of tag key:value pairs assigned to the channel.
getChannelResponse_tags :: Lens.Lens' GetChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getChannelResponse_tags = Lens.lens (\GetChannelResponse' {tags} -> tags) (\s@GetChannelResponse' {} a -> s {tags = a} :: GetChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getChannelResponse_httpStatus :: Lens.Lens' GetChannelResponse Prelude.Int
getChannelResponse_httpStatus = Lens.lens (\GetChannelResponse' {httpStatus} -> httpStatus) (\s@GetChannelResponse' {} a -> s {httpStatus = a} :: GetChannelResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
getChannelResponse_arn :: Lens.Lens' GetChannelResponse Prelude.Text
getChannelResponse_arn = Lens.lens (\GetChannelResponse' {arn} -> arn) (\s@GetChannelResponse' {} a -> s {arn = a} :: GetChannelResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getChannelResponse_channelName :: Lens.Lens' GetChannelResponse Prelude.Text
getChannelResponse_channelName = Lens.lens (\GetChannelResponse' {channelName} -> channelName) (\s@GetChannelResponse' {} a -> s {channelName = a} :: GetChannelResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getChannelResponse_channelGroupName :: Lens.Lens' GetChannelResponse Prelude.Text
getChannelResponse_channelGroupName = Lens.lens (\GetChannelResponse' {channelGroupName} -> channelGroupName) (\s@GetChannelResponse' {} a -> s {channelGroupName = a} :: GetChannelResponse)

-- | The date and time the channel was created.
getChannelResponse_createdAt :: Lens.Lens' GetChannelResponse Prelude.UTCTime
getChannelResponse_createdAt = Lens.lens (\GetChannelResponse' {createdAt} -> createdAt) (\s@GetChannelResponse' {} a -> s {createdAt = a} :: GetChannelResponse) Prelude.. Data._Time

-- | The date and time the channel was modified.
getChannelResponse_modifiedAt :: Lens.Lens' GetChannelResponse Prelude.UTCTime
getChannelResponse_modifiedAt = Lens.lens (\GetChannelResponse' {modifiedAt} -> modifiedAt) (\s@GetChannelResponse' {} a -> s {modifiedAt = a} :: GetChannelResponse) Prelude.. Data._Time

instance Prelude.NFData GetChannelResponse where
  rnf GetChannelResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ingestEndpoints
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
