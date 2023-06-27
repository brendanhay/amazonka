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
-- Module      : Amazonka.MediaPackageV2.CreateOriginEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The endpoint is attached to a channel, and represents the output of the
-- live content. You can associate multiple endpoints to a single channel.
-- Each endpoint gives players and downstream CDNs (such as Amazon
-- CloudFront) access to the content for playback. Content can\'t be served
-- from a channel until it has an endpoint. You can create only one
-- endpoint with each request.
module Amazonka.MediaPackageV2.CreateOriginEndpoint
  ( -- * Creating a Request
    CreateOriginEndpoint (..),
    newCreateOriginEndpoint,

    -- * Request Lenses
    createOriginEndpoint_clientToken,
    createOriginEndpoint_description,
    createOriginEndpoint_hlsManifests,
    createOriginEndpoint_lowLatencyHlsManifests,
    createOriginEndpoint_segment,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_tags,
    createOriginEndpoint_channelGroupName,
    createOriginEndpoint_channelName,
    createOriginEndpoint_originEndpointName,
    createOriginEndpoint_containerType,

    -- * Destructuring the Response
    CreateOriginEndpointResponse (..),
    newCreateOriginEndpointResponse,

    -- * Response Lenses
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_hlsManifests,
    createOriginEndpointResponse_lowLatencyHlsManifests,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_httpStatus,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_channelGroupName,
    createOriginEndpointResponse_channelName,
    createOriginEndpointResponse_originEndpointName,
    createOriginEndpointResponse_containerType,
    createOriginEndpointResponse_segment,
    createOriginEndpointResponse_createdAt,
    createOriginEndpointResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Enter any descriptive text that helps you to identify the origin
    -- endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | An HTTP live streaming (HLS) manifest configuration.
    hlsManifests :: Prelude.Maybe [CreateHlsManifestConfiguration],
    -- | A low-latency HLS manifest configuration.
    lowLatencyHlsManifests :: Prelude.Maybe [CreateLowLatencyHlsManifestConfiguration],
    -- | The segment configuration, including the segment name, duration, and
    -- other configuration values.
    segment :: Prelude.Maybe Segment,
    -- | The size of the window (in seconds) to create a window of the live
    -- stream that\'s available for on-demand viewing. Viewers can start-over
    -- or catch-up on content that falls within the window. The maximum
    -- startover window is 1,209,600 seconds (14 days).
    startoverWindowSeconds :: Prelude.Maybe Prelude.Natural,
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
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the origin endpoint. The name is the primary
    -- identifier for the origin endpoint, and must be unique for your account
    -- in the AWS Region and channel. You can\'t use spaces in the name. You
    -- can\'t change the name after you create the endpoint.
    originEndpointName :: Prelude.Text,
    -- | The type of container to attach to this origin endpoint. A container
    -- type is a file format that encapsulates one or more media streams, such
    -- as audio and video, into a single file. You can\'t change the container
    -- type after you create the endpoint.
    containerType :: ContainerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createOriginEndpoint_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createOriginEndpoint_description' - Enter any descriptive text that helps you to identify the origin
-- endpoint.
--
-- 'hlsManifests', 'createOriginEndpoint_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'createOriginEndpoint_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'segment', 'createOriginEndpoint_segment' - The segment configuration, including the segment name, duration, and
-- other configuration values.
--
-- 'startoverWindowSeconds', 'createOriginEndpoint_startoverWindowSeconds' - The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window. The maximum
-- startover window is 1,209,600 seconds (14 days).
--
-- 'tags', 'createOriginEndpoint_tags' - A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
--
-- 'channelGroupName', 'createOriginEndpoint_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'createOriginEndpoint_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'createOriginEndpoint_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and must be unique for your account
-- in the AWS Region and channel. You can\'t use spaces in the name. You
-- can\'t change the name after you create the endpoint.
--
-- 'containerType', 'createOriginEndpoint_containerType' - The type of container to attach to this origin endpoint. A container
-- type is a file format that encapsulates one or more media streams, such
-- as audio and video, into a single file. You can\'t change the container
-- type after you create the endpoint.
newCreateOriginEndpoint ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'containerType'
  ContainerType ->
  CreateOriginEndpoint
newCreateOriginEndpoint
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_ =
    CreateOriginEndpoint'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        hlsManifests = Prelude.Nothing,
        lowLatencyHlsManifests = Prelude.Nothing,
        segment = Prelude.Nothing,
        startoverWindowSeconds = Prelude.Nothing,
        tags = Prelude.Nothing,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        containerType = pContainerType_
      }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createOriginEndpoint_clientToken :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_clientToken = Lens.lens (\CreateOriginEndpoint' {clientToken} -> clientToken) (\s@CreateOriginEndpoint' {} a -> s {clientToken = a} :: CreateOriginEndpoint)

-- | Enter any descriptive text that helps you to identify the origin
-- endpoint.
createOriginEndpoint_description :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_description = Lens.lens (\CreateOriginEndpoint' {description} -> description) (\s@CreateOriginEndpoint' {} a -> s {description = a} :: CreateOriginEndpoint)

-- | An HTTP live streaming (HLS) manifest configuration.
createOriginEndpoint_hlsManifests :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe [CreateHlsManifestConfiguration])
createOriginEndpoint_hlsManifests = Lens.lens (\CreateOriginEndpoint' {hlsManifests} -> hlsManifests) (\s@CreateOriginEndpoint' {} a -> s {hlsManifests = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
createOriginEndpoint_lowLatencyHlsManifests :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe [CreateLowLatencyHlsManifestConfiguration])
createOriginEndpoint_lowLatencyHlsManifests = Lens.lens (\CreateOriginEndpoint' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@CreateOriginEndpoint' {} a -> s {lowLatencyHlsManifests = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The segment configuration, including the segment name, duration, and
-- other configuration values.
createOriginEndpoint_segment :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Segment)
createOriginEndpoint_segment = Lens.lens (\CreateOriginEndpoint' {segment} -> segment) (\s@CreateOriginEndpoint' {} a -> s {segment = a} :: CreateOriginEndpoint)

-- | The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window. The maximum
-- startover window is 1,209,600 seconds (14 days).
createOriginEndpoint_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Natural)
createOriginEndpoint_startoverWindowSeconds = Lens.lens (\CreateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpoint)

-- | A comma-separated list of tag key:value pairs that you define. For
-- example:
--
-- @\"Key1\": \"Value1\",@
--
-- @\"Key2\": \"Value2\"@
createOriginEndpoint_tags :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpoint_tags = Lens.lens (\CreateOriginEndpoint' {tags} -> tags) (\s@CreateOriginEndpoint' {} a -> s {tags = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
createOriginEndpoint_channelGroupName :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_channelGroupName = Lens.lens (\CreateOriginEndpoint' {channelGroupName} -> channelGroupName) (\s@CreateOriginEndpoint' {} a -> s {channelGroupName = a} :: CreateOriginEndpoint)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
createOriginEndpoint_channelName :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_channelName = Lens.lens (\CreateOriginEndpoint' {channelName} -> channelName) (\s@CreateOriginEndpoint' {} a -> s {channelName = a} :: CreateOriginEndpoint)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and must be unique for your account
-- in the AWS Region and channel. You can\'t use spaces in the name. You
-- can\'t change the name after you create the endpoint.
createOriginEndpoint_originEndpointName :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_originEndpointName = Lens.lens (\CreateOriginEndpoint' {originEndpointName} -> originEndpointName) (\s@CreateOriginEndpoint' {} a -> s {originEndpointName = a} :: CreateOriginEndpoint)

-- | The type of container to attach to this origin endpoint. A container
-- type is a file format that encapsulates one or more media streams, such
-- as audio and video, into a single file. You can\'t change the container
-- type after you create the endpoint.
createOriginEndpoint_containerType :: Lens.Lens' CreateOriginEndpoint ContainerType
createOriginEndpoint_containerType = Lens.lens (\CreateOriginEndpoint' {containerType} -> containerType) (\s@CreateOriginEndpoint' {} a -> s {containerType = a} :: CreateOriginEndpoint)

instance Core.AWSRequest CreateOriginEndpoint where
  type
    AWSResponse CreateOriginEndpoint =
      CreateOriginEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOriginEndpointResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "HlsManifests" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "LowLatencyHlsManifests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "StartoverWindowSeconds")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "ChannelGroupName")
            Prelude.<*> (x Data..:> "ChannelName")
            Prelude.<*> (x Data..:> "OriginEndpointName")
            Prelude.<*> (x Data..:> "ContainerType")
            Prelude.<*> (x Data..:> "Segment")
            Prelude.<*> (x Data..:> "CreatedAt")
            Prelude.<*> (x Data..:> "ModifiedAt")
      )

instance Prelude.Hashable CreateOriginEndpoint where
  hashWithSalt _salt CreateOriginEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hlsManifests
      `Prelude.hashWithSalt` lowLatencyHlsManifests
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` startoverWindowSeconds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName
      `Prelude.hashWithSalt` containerType

instance Prelude.NFData CreateOriginEndpoint where
  rnf CreateOriginEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf lowLatencyHlsManifests
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf containerType

instance Data.ToHeaders CreateOriginEndpoint where
  toHeaders CreateOriginEndpoint' {..} =
    Prelude.mconcat
      [ "x-amzn-client-token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("HlsManifests" Data..=) Prelude.<$> hlsManifests,
            ("LowLatencyHlsManifests" Data..=)
              Prelude.<$> lowLatencyHlsManifests,
            ("Segment" Data..=) Prelude.<$> segment,
            ("StartoverWindowSeconds" Data..=)
              Prelude.<$> startoverWindowSeconds,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("OriginEndpointName" Data..= originEndpointName),
            Prelude.Just
              ("ContainerType" Data..= containerType)
          ]
      )

instance Data.ToPath CreateOriginEndpoint where
  toPath CreateOriginEndpoint' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint"
      ]

instance Data.ToQuery CreateOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
  { -- | The description for your origin endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | An HTTP live streaming (HLS) manifest configuration.
    hlsManifests :: Prelude.Maybe [GetHlsManifestConfiguration],
    -- | A low-latency HLS manifest configuration.
    lowLatencyHlsManifests :: Prelude.Maybe [GetLowLatencyHlsManifestConfiguration],
    -- | The size of the window (in seconds) to create a window of the live
    -- stream that\'s available for on-demand viewing. Viewers can start-over
    -- or catch-up on content that falls within the window.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | The comma-separated list of tag key:value pairs assigned to the origin
    -- endpoint.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the origin endpoint. The name is the primary
    -- identifier for the origin endpoint, and and must be unique for your
    -- account in the AWS Region and channel.
    originEndpointName :: Prelude.Text,
    -- | The type of container attached to this origin endpoint.
    containerType :: ContainerType,
    -- | The segment configuration, including the segment name, duration, and
    -- other configuration values.
    segment :: Segment,
    -- | The date and time the origin endpoint was created.
    createdAt :: Data.POSIX,
    -- | The date and time the origin endpoint was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createOriginEndpointResponse_description' - The description for your origin endpoint.
--
-- 'hlsManifests', 'createOriginEndpointResponse_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'createOriginEndpointResponse_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'startoverWindowSeconds', 'createOriginEndpointResponse_startoverWindowSeconds' - The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
--
-- 'tags', 'createOriginEndpointResponse_tags' - The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
--
-- 'httpStatus', 'createOriginEndpointResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelGroupName', 'createOriginEndpointResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'createOriginEndpointResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'createOriginEndpointResponse_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'containerType', 'createOriginEndpointResponse_containerType' - The type of container attached to this origin endpoint.
--
-- 'segment', 'createOriginEndpointResponse_segment' - The segment configuration, including the segment name, duration, and
-- other configuration values.
--
-- 'createdAt', 'createOriginEndpointResponse_createdAt' - The date and time the origin endpoint was created.
--
-- 'modifiedAt', 'createOriginEndpointResponse_modifiedAt' - The date and time the origin endpoint was modified.
newCreateOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'containerType'
  ContainerType ->
  -- | 'segment'
  Segment ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  CreateOriginEndpointResponse
newCreateOriginEndpointResponse
  pHttpStatus_
  pArn_
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_
  pSegment_
  pCreatedAt_
  pModifiedAt_ =
    CreateOriginEndpointResponse'
      { description =
          Prelude.Nothing,
        hlsManifests = Prelude.Nothing,
        lowLatencyHlsManifests = Prelude.Nothing,
        startoverWindowSeconds = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        containerType = pContainerType_,
        segment = pSegment_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | The description for your origin endpoint.
createOriginEndpointResponse_description :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_description = Lens.lens (\CreateOriginEndpointResponse' {description} -> description) (\s@CreateOriginEndpointResponse' {} a -> s {description = a} :: CreateOriginEndpointResponse)

-- | An HTTP live streaming (HLS) manifest configuration.
createOriginEndpointResponse_hlsManifests :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe [GetHlsManifestConfiguration])
createOriginEndpointResponse_hlsManifests = Lens.lens (\CreateOriginEndpointResponse' {hlsManifests} -> hlsManifests) (\s@CreateOriginEndpointResponse' {} a -> s {hlsManifests = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
createOriginEndpointResponse_lowLatencyHlsManifests :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe [GetLowLatencyHlsManifestConfiguration])
createOriginEndpointResponse_lowLatencyHlsManifests = Lens.lens (\CreateOriginEndpointResponse' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@CreateOriginEndpointResponse' {} a -> s {lowLatencyHlsManifests = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
createOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
createOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\CreateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpointResponse)

-- | The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
createOriginEndpointResponse_tags :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpointResponse_tags = Lens.lens (\CreateOriginEndpointResponse' {tags} -> tags) (\s@CreateOriginEndpointResponse' {} a -> s {tags = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createOriginEndpointResponse_httpStatus :: Lens.Lens' CreateOriginEndpointResponse Prelude.Int
createOriginEndpointResponse_httpStatus = Lens.lens (\CreateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateOriginEndpointResponse' {} a -> s {httpStatus = a} :: CreateOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
createOriginEndpointResponse_arn :: Lens.Lens' CreateOriginEndpointResponse Prelude.Text
createOriginEndpointResponse_arn = Lens.lens (\CreateOriginEndpointResponse' {arn} -> arn) (\s@CreateOriginEndpointResponse' {} a -> s {arn = a} :: CreateOriginEndpointResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
createOriginEndpointResponse_channelGroupName :: Lens.Lens' CreateOriginEndpointResponse Prelude.Text
createOriginEndpointResponse_channelGroupName = Lens.lens (\CreateOriginEndpointResponse' {channelGroupName} -> channelGroupName) (\s@CreateOriginEndpointResponse' {} a -> s {channelGroupName = a} :: CreateOriginEndpointResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
createOriginEndpointResponse_channelName :: Lens.Lens' CreateOriginEndpointResponse Prelude.Text
createOriginEndpointResponse_channelName = Lens.lens (\CreateOriginEndpointResponse' {channelName} -> channelName) (\s@CreateOriginEndpointResponse' {} a -> s {channelName = a} :: CreateOriginEndpointResponse)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
createOriginEndpointResponse_originEndpointName :: Lens.Lens' CreateOriginEndpointResponse Prelude.Text
createOriginEndpointResponse_originEndpointName = Lens.lens (\CreateOriginEndpointResponse' {originEndpointName} -> originEndpointName) (\s@CreateOriginEndpointResponse' {} a -> s {originEndpointName = a} :: CreateOriginEndpointResponse)

-- | The type of container attached to this origin endpoint.
createOriginEndpointResponse_containerType :: Lens.Lens' CreateOriginEndpointResponse ContainerType
createOriginEndpointResponse_containerType = Lens.lens (\CreateOriginEndpointResponse' {containerType} -> containerType) (\s@CreateOriginEndpointResponse' {} a -> s {containerType = a} :: CreateOriginEndpointResponse)

-- | The segment configuration, including the segment name, duration, and
-- other configuration values.
createOriginEndpointResponse_segment :: Lens.Lens' CreateOriginEndpointResponse Segment
createOriginEndpointResponse_segment = Lens.lens (\CreateOriginEndpointResponse' {segment} -> segment) (\s@CreateOriginEndpointResponse' {} a -> s {segment = a} :: CreateOriginEndpointResponse)

-- | The date and time the origin endpoint was created.
createOriginEndpointResponse_createdAt :: Lens.Lens' CreateOriginEndpointResponse Prelude.UTCTime
createOriginEndpointResponse_createdAt = Lens.lens (\CreateOriginEndpointResponse' {createdAt} -> createdAt) (\s@CreateOriginEndpointResponse' {} a -> s {createdAt = a} :: CreateOriginEndpointResponse) Prelude.. Data._Time

-- | The date and time the origin endpoint was modified.
createOriginEndpointResponse_modifiedAt :: Lens.Lens' CreateOriginEndpointResponse Prelude.UTCTime
createOriginEndpointResponse_modifiedAt = Lens.lens (\CreateOriginEndpointResponse' {modifiedAt} -> modifiedAt) (\s@CreateOriginEndpointResponse' {} a -> s {modifiedAt = a} :: CreateOriginEndpointResponse) Prelude.. Data._Time

instance Prelude.NFData CreateOriginEndpointResponse where
  rnf CreateOriginEndpointResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf lowLatencyHlsManifests
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf containerType
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
