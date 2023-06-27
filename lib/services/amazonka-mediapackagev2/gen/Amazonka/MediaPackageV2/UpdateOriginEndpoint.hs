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
-- Module      : Amazonka.MediaPackageV2.UpdateOriginEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the specified origin endpoint. Edit the packaging preferences on
-- an endpoint to optimize the viewing experience. You can\'t edit the name
-- of the endpoint.
--
-- Any edits you make that impact the video output may not be reflected for
-- a few minutes.
module Amazonka.MediaPackageV2.UpdateOriginEndpoint
  ( -- * Creating a Request
    UpdateOriginEndpoint (..),
    newUpdateOriginEndpoint,

    -- * Request Lenses
    updateOriginEndpoint_description,
    updateOriginEndpoint_hlsManifests,
    updateOriginEndpoint_lowLatencyHlsManifests,
    updateOriginEndpoint_segment,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_channelGroupName,
    updateOriginEndpoint_channelName,
    updateOriginEndpoint_originEndpointName,
    updateOriginEndpoint_containerType,

    -- * Destructuring the Response
    UpdateOriginEndpointResponse (..),
    newUpdateOriginEndpointResponse,

    -- * Response Lenses
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_hlsManifests,
    updateOriginEndpointResponse_lowLatencyHlsManifests,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_httpStatus,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_channelGroupName,
    updateOriginEndpointResponse_channelName,
    updateOriginEndpointResponse_originEndpointName,
    updateOriginEndpointResponse_containerType,
    updateOriginEndpointResponse_segment,
    updateOriginEndpointResponse_createdAt,
    updateOriginEndpointResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { -- | Any descriptive information that you want to add to the origin endpoint
    -- for future identification purposes.
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
    -- | The type of container attached to this origin endpoint. A container type
    -- is a file format that encapsulates one or more media streams, such as
    -- audio and video, into a single file.
    containerType :: ContainerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateOriginEndpoint_description' - Any descriptive information that you want to add to the origin endpoint
-- for future identification purposes.
--
-- 'hlsManifests', 'updateOriginEndpoint_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'updateOriginEndpoint_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'segment', 'updateOriginEndpoint_segment' - The segment configuration, including the segment name, duration, and
-- other configuration values.
--
-- 'startoverWindowSeconds', 'updateOriginEndpoint_startoverWindowSeconds' - The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window. The maximum
-- startover window is 1,209,600 seconds (14 days).
--
-- 'channelGroupName', 'updateOriginEndpoint_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'updateOriginEndpoint_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'updateOriginEndpoint_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'containerType', 'updateOriginEndpoint_containerType' - The type of container attached to this origin endpoint. A container type
-- is a file format that encapsulates one or more media streams, such as
-- audio and video, into a single file.
newUpdateOriginEndpoint ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'containerType'
  ContainerType ->
  UpdateOriginEndpoint
newUpdateOriginEndpoint
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_ =
    UpdateOriginEndpoint'
      { description =
          Prelude.Nothing,
        hlsManifests = Prelude.Nothing,
        lowLatencyHlsManifests = Prelude.Nothing,
        segment = Prelude.Nothing,
        startoverWindowSeconds = Prelude.Nothing,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        containerType = pContainerType_
      }

-- | Any descriptive information that you want to add to the origin endpoint
-- for future identification purposes.
updateOriginEndpoint_description :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Text)
updateOriginEndpoint_description = Lens.lens (\UpdateOriginEndpoint' {description} -> description) (\s@UpdateOriginEndpoint' {} a -> s {description = a} :: UpdateOriginEndpoint)

-- | An HTTP live streaming (HLS) manifest configuration.
updateOriginEndpoint_hlsManifests :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe [CreateHlsManifestConfiguration])
updateOriginEndpoint_hlsManifests = Lens.lens (\UpdateOriginEndpoint' {hlsManifests} -> hlsManifests) (\s@UpdateOriginEndpoint' {} a -> s {hlsManifests = a} :: UpdateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
updateOriginEndpoint_lowLatencyHlsManifests :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe [CreateLowLatencyHlsManifestConfiguration])
updateOriginEndpoint_lowLatencyHlsManifests = Lens.lens (\UpdateOriginEndpoint' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@UpdateOriginEndpoint' {} a -> s {lowLatencyHlsManifests = a} :: UpdateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The segment configuration, including the segment name, duration, and
-- other configuration values.
updateOriginEndpoint_segment :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Segment)
updateOriginEndpoint_segment = Lens.lens (\UpdateOriginEndpoint' {segment} -> segment) (\s@UpdateOriginEndpoint' {} a -> s {segment = a} :: UpdateOriginEndpoint)

-- | The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window. The maximum
-- startover window is 1,209,600 seconds (14 days).
updateOriginEndpoint_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Natural)
updateOriginEndpoint_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpoint)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateOriginEndpoint_channelGroupName :: Lens.Lens' UpdateOriginEndpoint Prelude.Text
updateOriginEndpoint_channelGroupName = Lens.lens (\UpdateOriginEndpoint' {channelGroupName} -> channelGroupName) (\s@UpdateOriginEndpoint' {} a -> s {channelGroupName = a} :: UpdateOriginEndpoint)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
updateOriginEndpoint_channelName :: Lens.Lens' UpdateOriginEndpoint Prelude.Text
updateOriginEndpoint_channelName = Lens.lens (\UpdateOriginEndpoint' {channelName} -> channelName) (\s@UpdateOriginEndpoint' {} a -> s {channelName = a} :: UpdateOriginEndpoint)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
updateOriginEndpoint_originEndpointName :: Lens.Lens' UpdateOriginEndpoint Prelude.Text
updateOriginEndpoint_originEndpointName = Lens.lens (\UpdateOriginEndpoint' {originEndpointName} -> originEndpointName) (\s@UpdateOriginEndpoint' {} a -> s {originEndpointName = a} :: UpdateOriginEndpoint)

-- | The type of container attached to this origin endpoint. A container type
-- is a file format that encapsulates one or more media streams, such as
-- audio and video, into a single file.
updateOriginEndpoint_containerType :: Lens.Lens' UpdateOriginEndpoint ContainerType
updateOriginEndpoint_containerType = Lens.lens (\UpdateOriginEndpoint' {containerType} -> containerType) (\s@UpdateOriginEndpoint' {} a -> s {containerType = a} :: UpdateOriginEndpoint)

instance Core.AWSRequest UpdateOriginEndpoint where
  type
    AWSResponse UpdateOriginEndpoint =
      UpdateOriginEndpointResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOriginEndpointResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "HlsManifests" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "LowLatencyHlsManifests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "StartoverWindowSeconds")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
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

instance Prelude.Hashable UpdateOriginEndpoint where
  hashWithSalt _salt UpdateOriginEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hlsManifests
      `Prelude.hashWithSalt` lowLatencyHlsManifests
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` startoverWindowSeconds
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName
      `Prelude.hashWithSalt` containerType

instance Prelude.NFData UpdateOriginEndpoint where
  rnf UpdateOriginEndpoint' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf lowLatencyHlsManifests
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf containerType

instance Data.ToHeaders UpdateOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateOriginEndpoint where
  toJSON UpdateOriginEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("HlsManifests" Data..=) Prelude.<$> hlsManifests,
            ("LowLatencyHlsManifests" Data..=)
              Prelude.<$> lowLatencyHlsManifests,
            ("Segment" Data..=) Prelude.<$> segment,
            ("StartoverWindowSeconds" Data..=)
              Prelude.<$> startoverWindowSeconds,
            Prelude.Just
              ("ContainerType" Data..= containerType)
          ]
      )

instance Data.ToPath UpdateOriginEndpoint where
  toPath UpdateOriginEndpoint' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName
      ]

instance Data.ToQuery UpdateOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
  { -- | The description of the origin endpoint.
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
    -- | The ARN associated with the resource.
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
-- Create a value of 'UpdateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateOriginEndpointResponse_description' - The description of the origin endpoint.
--
-- 'hlsManifests', 'updateOriginEndpointResponse_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'updateOriginEndpointResponse_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'startoverWindowSeconds', 'updateOriginEndpointResponse_startoverWindowSeconds' - The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
--
-- 'tags', 'updateOriginEndpointResponse_tags' - The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
--
-- 'httpStatus', 'updateOriginEndpointResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateOriginEndpointResponse_arn' - The ARN associated with the resource.
--
-- 'channelGroupName', 'updateOriginEndpointResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'updateOriginEndpointResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'updateOriginEndpointResponse_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'containerType', 'updateOriginEndpointResponse_containerType' - The type of container attached to this origin endpoint.
--
-- 'segment', 'updateOriginEndpointResponse_segment' - The segment configuration, including the segment name, duration, and
-- other configuration values.
--
-- 'createdAt', 'updateOriginEndpointResponse_createdAt' - The date and time the origin endpoint was created.
--
-- 'modifiedAt', 'updateOriginEndpointResponse_modifiedAt' - The date and time the origin endpoint was modified.
newUpdateOriginEndpointResponse ::
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
  UpdateOriginEndpointResponse
newUpdateOriginEndpointResponse
  pHttpStatus_
  pArn_
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_
  pSegment_
  pCreatedAt_
  pModifiedAt_ =
    UpdateOriginEndpointResponse'
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

-- | The description of the origin endpoint.
updateOriginEndpointResponse_description :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_description = Lens.lens (\UpdateOriginEndpointResponse' {description} -> description) (\s@UpdateOriginEndpointResponse' {} a -> s {description = a} :: UpdateOriginEndpointResponse)

-- | An HTTP live streaming (HLS) manifest configuration.
updateOriginEndpointResponse_hlsManifests :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe [GetHlsManifestConfiguration])
updateOriginEndpointResponse_hlsManifests = Lens.lens (\UpdateOriginEndpointResponse' {hlsManifests} -> hlsManifests) (\s@UpdateOriginEndpointResponse' {} a -> s {hlsManifests = a} :: UpdateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
updateOriginEndpointResponse_lowLatencyHlsManifests :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe [GetLowLatencyHlsManifestConfiguration])
updateOriginEndpointResponse_lowLatencyHlsManifests = Lens.lens (\UpdateOriginEndpointResponse' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@UpdateOriginEndpointResponse' {} a -> s {lowLatencyHlsManifests = a} :: UpdateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
updateOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
updateOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpointResponse)

-- | The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
updateOriginEndpointResponse_tags :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateOriginEndpointResponse_tags = Lens.lens (\UpdateOriginEndpointResponse' {tags} -> tags) (\s@UpdateOriginEndpointResponse' {} a -> s {tags = a} :: UpdateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateOriginEndpointResponse_httpStatus :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Int
updateOriginEndpointResponse_httpStatus = Lens.lens (\UpdateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateOriginEndpointResponse' {} a -> s {httpStatus = a} :: UpdateOriginEndpointResponse)

-- | The ARN associated with the resource.
updateOriginEndpointResponse_arn :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Text
updateOriginEndpointResponse_arn = Lens.lens (\UpdateOriginEndpointResponse' {arn} -> arn) (\s@UpdateOriginEndpointResponse' {} a -> s {arn = a} :: UpdateOriginEndpointResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
updateOriginEndpointResponse_channelGroupName :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Text
updateOriginEndpointResponse_channelGroupName = Lens.lens (\UpdateOriginEndpointResponse' {channelGroupName} -> channelGroupName) (\s@UpdateOriginEndpointResponse' {} a -> s {channelGroupName = a} :: UpdateOriginEndpointResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
updateOriginEndpointResponse_channelName :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Text
updateOriginEndpointResponse_channelName = Lens.lens (\UpdateOriginEndpointResponse' {channelName} -> channelName) (\s@UpdateOriginEndpointResponse' {} a -> s {channelName = a} :: UpdateOriginEndpointResponse)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
updateOriginEndpointResponse_originEndpointName :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Text
updateOriginEndpointResponse_originEndpointName = Lens.lens (\UpdateOriginEndpointResponse' {originEndpointName} -> originEndpointName) (\s@UpdateOriginEndpointResponse' {} a -> s {originEndpointName = a} :: UpdateOriginEndpointResponse)

-- | The type of container attached to this origin endpoint.
updateOriginEndpointResponse_containerType :: Lens.Lens' UpdateOriginEndpointResponse ContainerType
updateOriginEndpointResponse_containerType = Lens.lens (\UpdateOriginEndpointResponse' {containerType} -> containerType) (\s@UpdateOriginEndpointResponse' {} a -> s {containerType = a} :: UpdateOriginEndpointResponse)

-- | The segment configuration, including the segment name, duration, and
-- other configuration values.
updateOriginEndpointResponse_segment :: Lens.Lens' UpdateOriginEndpointResponse Segment
updateOriginEndpointResponse_segment = Lens.lens (\UpdateOriginEndpointResponse' {segment} -> segment) (\s@UpdateOriginEndpointResponse' {} a -> s {segment = a} :: UpdateOriginEndpointResponse)

-- | The date and time the origin endpoint was created.
updateOriginEndpointResponse_createdAt :: Lens.Lens' UpdateOriginEndpointResponse Prelude.UTCTime
updateOriginEndpointResponse_createdAt = Lens.lens (\UpdateOriginEndpointResponse' {createdAt} -> createdAt) (\s@UpdateOriginEndpointResponse' {} a -> s {createdAt = a} :: UpdateOriginEndpointResponse) Prelude.. Data._Time

-- | The date and time the origin endpoint was modified.
updateOriginEndpointResponse_modifiedAt :: Lens.Lens' UpdateOriginEndpointResponse Prelude.UTCTime
updateOriginEndpointResponse_modifiedAt = Lens.lens (\UpdateOriginEndpointResponse' {modifiedAt} -> modifiedAt) (\s@UpdateOriginEndpointResponse' {} a -> s {modifiedAt = a} :: UpdateOriginEndpointResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateOriginEndpointResponse where
  rnf UpdateOriginEndpointResponse' {..} =
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
