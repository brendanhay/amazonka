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
-- Module      : Amazonka.MediaPackageV2.UpdateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the specified channel. You can edit if MediaPackage sends ingest
-- or egress access logs to the CloudWatch log group, if content will be
-- encrypted, the description on a channel, and your channel\'s policy
-- settings. You can\'t edit the name of the channel or CloudFront
-- distribution details.
--
-- Any edits you make that impact the video output may not be reflected for
-- a few minutes.
module Amazonka.MediaPackageV2.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel (..),
    newUpdateChannel,

    -- * Request Lenses
    updateChannel_description,
    updateChannel_channelGroupName,
    updateChannel_channelName,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_description,
    updateChannelResponse_ingestEndpoints,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,
    updateChannelResponse_arn,
    updateChannelResponse_channelName,
    updateChannelResponse_channelGroupName,
    updateChannelResponse_createdAt,
    updateChannelResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | Any descriptive information that you want to add to the channel for
    -- future identification purposes.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
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
-- Create a value of 'UpdateChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateChannel_description' - Any descriptive information that you want to add to the channel for
-- future identification purposes.
--
-- 'channelGroupName', 'updateChannel_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'updateChannel_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
newUpdateChannel ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  UpdateChannel
newUpdateChannel pChannelGroupName_ pChannelName_ =
  UpdateChannel'
    { description = Prelude.Nothing,
      channelGroupName = pChannelGroupName_,
      channelName = pChannelName_
    }

-- | Any descriptive information that you want to add to the channel for
-- future identification purposes.
updateChannel_description :: Lens.Lens' UpdateChannel (Prelude.Maybe Prelude.Text)
updateChannel_description = Lens.lens (\UpdateChannel' {description} -> description) (\s@UpdateChannel' {} a -> s {description = a} :: UpdateChannel)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateChannel_channelGroupName :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_channelGroupName = Lens.lens (\UpdateChannel' {channelGroupName} -> channelGroupName) (\s@UpdateChannel' {} a -> s {channelGroupName = a} :: UpdateChannel)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
updateChannel_channelName :: Lens.Lens' UpdateChannel Prelude.Text
updateChannel_channelName = Lens.lens (\UpdateChannel' {channelName} -> channelName) (\s@UpdateChannel' {} a -> s {channelName = a} :: UpdateChannel)

instance Core.AWSRequest UpdateChannel where
  type
    AWSResponse UpdateChannel =
      UpdateChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> ( x
                            Data..?> "IngestEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "ChannelName")
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable UpdateChannel where
  hashWithSalt _salt UpdateChannel' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData UpdateChannel where
  rnf UpdateChannel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName

instance Data.ToHeaders UpdateChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/"
      ]

instance Data.ToQuery UpdateChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
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
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateChannelResponse_description' - The description for your channel.
--
-- 'ingestEndpoints', 'updateChannelResponse_ingestEndpoints' - Undocumented member.
--
-- 'tags', 'updateChannelResponse_tags' - The comma-separated list of tag key:value pairs assigned to the channel.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateChannelResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelName', 'updateChannelResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'channelGroupName', 'updateChannelResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'createdAt', 'updateChannelResponse_createdAt' - The date and time the channel was created.
--
-- 'modifiedAt', 'updateChannelResponse_modifiedAt' - The date and time the channel was modified.
newUpdateChannelResponse ::
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
  UpdateChannelResponse
newUpdateChannelResponse
  pHttpStatus_
  pArn_
  pChannelName_
  pChannelGroupName_
  pCreatedAt_
  pModifiedAt_ =
    UpdateChannelResponse'
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
updateChannelResponse_description :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Prelude.Text)
updateChannelResponse_description = Lens.lens (\UpdateChannelResponse' {description} -> description) (\s@UpdateChannelResponse' {} a -> s {description = a} :: UpdateChannelResponse)

-- | Undocumented member.
updateChannelResponse_ingestEndpoints :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe [IngestEndpoint])
updateChannelResponse_ingestEndpoints = Lens.lens (\UpdateChannelResponse' {ingestEndpoints} -> ingestEndpoints) (\s@UpdateChannelResponse' {} a -> s {ingestEndpoints = a} :: UpdateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The comma-separated list of tag key:value pairs assigned to the channel.
updateChannelResponse_tags :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateChannelResponse_tags = Lens.lens (\UpdateChannelResponse' {tags} -> tags) (\s@UpdateChannelResponse' {} a -> s {tags = a} :: UpdateChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
updateChannelResponse_arn :: Lens.Lens' UpdateChannelResponse Prelude.Text
updateChannelResponse_arn = Lens.lens (\UpdateChannelResponse' {arn} -> arn) (\s@UpdateChannelResponse' {} a -> s {arn = a} :: UpdateChannelResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
updateChannelResponse_channelName :: Lens.Lens' UpdateChannelResponse Prelude.Text
updateChannelResponse_channelName = Lens.lens (\UpdateChannelResponse' {channelName} -> channelName) (\s@UpdateChannelResponse' {} a -> s {channelName = a} :: UpdateChannelResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateChannelResponse_channelGroupName :: Lens.Lens' UpdateChannelResponse Prelude.Text
updateChannelResponse_channelGroupName = Lens.lens (\UpdateChannelResponse' {channelGroupName} -> channelGroupName) (\s@UpdateChannelResponse' {} a -> s {channelGroupName = a} :: UpdateChannelResponse)

-- | The date and time the channel was created.
updateChannelResponse_createdAt :: Lens.Lens' UpdateChannelResponse Prelude.UTCTime
updateChannelResponse_createdAt = Lens.lens (\UpdateChannelResponse' {createdAt} -> createdAt) (\s@UpdateChannelResponse' {} a -> s {createdAt = a} :: UpdateChannelResponse) Prelude.. Data._Time

-- | The date and time the channel was modified.
updateChannelResponse_modifiedAt :: Lens.Lens' UpdateChannelResponse Prelude.UTCTime
updateChannelResponse_modifiedAt = Lens.lens (\UpdateChannelResponse' {modifiedAt} -> modifiedAt) (\s@UpdateChannelResponse' {} a -> s {modifiedAt = a} :: UpdateChannelResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateChannelResponse where
  rnf UpdateChannelResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ingestEndpoints
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
