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
-- Module      : Amazonka.MediaPackageV2.GetOriginEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified origin endpoint that\'s configured in AWS
-- Elemental MediaPackage to obtain its playback URL and to view the
-- packaging settings that it\'s currently using.
module Amazonka.MediaPackageV2.GetOriginEndpoint
  ( -- * Creating a Request
    GetOriginEndpoint (..),
    newGetOriginEndpoint,

    -- * Request Lenses
    getOriginEndpoint_channelGroupName,
    getOriginEndpoint_channelName,
    getOriginEndpoint_originEndpointName,

    -- * Destructuring the Response
    GetOriginEndpointResponse (..),
    newGetOriginEndpointResponse,

    -- * Response Lenses
    getOriginEndpointResponse_description,
    getOriginEndpointResponse_hlsManifests,
    getOriginEndpointResponse_lowLatencyHlsManifests,
    getOriginEndpointResponse_startoverWindowSeconds,
    getOriginEndpointResponse_tags,
    getOriginEndpointResponse_httpStatus,
    getOriginEndpointResponse_arn,
    getOriginEndpointResponse_channelGroupName,
    getOriginEndpointResponse_channelName,
    getOriginEndpointResponse_originEndpointName,
    getOriginEndpointResponse_containerType,
    getOriginEndpointResponse_segment,
    getOriginEndpointResponse_createdAt,
    getOriginEndpointResponse_modifiedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginEndpoint' smart constructor.
data GetOriginEndpoint = GetOriginEndpoint'
  { -- | The name that describes the channel group. The name is the primary
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
    originEndpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelGroupName', 'getOriginEndpoint_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getOriginEndpoint_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'getOriginEndpoint_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
newGetOriginEndpoint ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  GetOriginEndpoint
newGetOriginEndpoint
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_ =
    GetOriginEndpoint'
      { channelGroupName =
          pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_
      }

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getOriginEndpoint_channelGroupName :: Lens.Lens' GetOriginEndpoint Prelude.Text
getOriginEndpoint_channelGroupName = Lens.lens (\GetOriginEndpoint' {channelGroupName} -> channelGroupName) (\s@GetOriginEndpoint' {} a -> s {channelGroupName = a} :: GetOriginEndpoint)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getOriginEndpoint_channelName :: Lens.Lens' GetOriginEndpoint Prelude.Text
getOriginEndpoint_channelName = Lens.lens (\GetOriginEndpoint' {channelName} -> channelName) (\s@GetOriginEndpoint' {} a -> s {channelName = a} :: GetOriginEndpoint)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
getOriginEndpoint_originEndpointName :: Lens.Lens' GetOriginEndpoint Prelude.Text
getOriginEndpoint_originEndpointName = Lens.lens (\GetOriginEndpoint' {originEndpointName} -> originEndpointName) (\s@GetOriginEndpoint' {} a -> s {originEndpointName = a} :: GetOriginEndpoint)

instance Core.AWSRequest GetOriginEndpoint where
  type
    AWSResponse GetOriginEndpoint =
      GetOriginEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOriginEndpointResponse'
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

instance Prelude.Hashable GetOriginEndpoint where
  hashWithSalt _salt GetOriginEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` originEndpointName

instance Prelude.NFData GetOriginEndpoint where
  rnf GetOriginEndpoint' {..} =
    Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName

instance Data.ToHeaders GetOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetOriginEndpoint where
  toPath GetOriginEndpoint' {..} =
    Prelude.mconcat
      [ "/channelGroup/",
        Data.toBS channelGroupName,
        "/channel/",
        Data.toBS channelName,
        "/originEndpoint/",
        Data.toBS originEndpointName
      ]

instance Data.ToQuery GetOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginEndpointResponse' smart constructor.
data GetOriginEndpointResponse = GetOriginEndpointResponse'
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
    segment :: Segment,
    -- | The date and time the origin endpoint was created.
    createdAt :: Data.POSIX,
    -- | The date and time the origin endpoint was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getOriginEndpointResponse_description' - The description for your origin endpoint.
--
-- 'hlsManifests', 'getOriginEndpointResponse_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'getOriginEndpointResponse_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'startoverWindowSeconds', 'getOriginEndpointResponse_startoverWindowSeconds' - The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
--
-- 'tags', 'getOriginEndpointResponse_tags' - The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
--
-- 'httpStatus', 'getOriginEndpointResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelGroupName', 'getOriginEndpointResponse_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'getOriginEndpointResponse_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'getOriginEndpointResponse_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'containerType', 'getOriginEndpointResponse_containerType' - The type of container attached to this origin endpoint.
--
-- 'segment', 'getOriginEndpointResponse_segment' - Undocumented member.
--
-- 'createdAt', 'getOriginEndpointResponse_createdAt' - The date and time the origin endpoint was created.
--
-- 'modifiedAt', 'getOriginEndpointResponse_modifiedAt' - The date and time the origin endpoint was modified.
newGetOriginEndpointResponse ::
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
  GetOriginEndpointResponse
newGetOriginEndpointResponse
  pHttpStatus_
  pArn_
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_
  pSegment_
  pCreatedAt_
  pModifiedAt_ =
    GetOriginEndpointResponse'
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
getOriginEndpointResponse_description :: Lens.Lens' GetOriginEndpointResponse (Prelude.Maybe Prelude.Text)
getOriginEndpointResponse_description = Lens.lens (\GetOriginEndpointResponse' {description} -> description) (\s@GetOriginEndpointResponse' {} a -> s {description = a} :: GetOriginEndpointResponse)

-- | An HTTP live streaming (HLS) manifest configuration.
getOriginEndpointResponse_hlsManifests :: Lens.Lens' GetOriginEndpointResponse (Prelude.Maybe [GetHlsManifestConfiguration])
getOriginEndpointResponse_hlsManifests = Lens.lens (\GetOriginEndpointResponse' {hlsManifests} -> hlsManifests) (\s@GetOriginEndpointResponse' {} a -> s {hlsManifests = a} :: GetOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
getOriginEndpointResponse_lowLatencyHlsManifests :: Lens.Lens' GetOriginEndpointResponse (Prelude.Maybe [GetLowLatencyHlsManifestConfiguration])
getOriginEndpointResponse_lowLatencyHlsManifests = Lens.lens (\GetOriginEndpointResponse' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@GetOriginEndpointResponse' {} a -> s {lowLatencyHlsManifests = a} :: GetOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The size of the window (in seconds) to create a window of the live
-- stream that\'s available for on-demand viewing. Viewers can start-over
-- or catch-up on content that falls within the window.
getOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' GetOriginEndpointResponse (Prelude.Maybe Prelude.Int)
getOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\GetOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@GetOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: GetOriginEndpointResponse)

-- | The comma-separated list of tag key:value pairs assigned to the origin
-- endpoint.
getOriginEndpointResponse_tags :: Lens.Lens' GetOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getOriginEndpointResponse_tags = Lens.lens (\GetOriginEndpointResponse' {tags} -> tags) (\s@GetOriginEndpointResponse' {} a -> s {tags = a} :: GetOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOriginEndpointResponse_httpStatus :: Lens.Lens' GetOriginEndpointResponse Prelude.Int
getOriginEndpointResponse_httpStatus = Lens.lens (\GetOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@GetOriginEndpointResponse' {} a -> s {httpStatus = a} :: GetOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) associated with the resource.
getOriginEndpointResponse_arn :: Lens.Lens' GetOriginEndpointResponse Prelude.Text
getOriginEndpointResponse_arn = Lens.lens (\GetOriginEndpointResponse' {arn} -> arn) (\s@GetOriginEndpointResponse' {} a -> s {arn = a} :: GetOriginEndpointResponse)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
getOriginEndpointResponse_channelGroupName :: Lens.Lens' GetOriginEndpointResponse Prelude.Text
getOriginEndpointResponse_channelGroupName = Lens.lens (\GetOriginEndpointResponse' {channelGroupName} -> channelGroupName) (\s@GetOriginEndpointResponse' {} a -> s {channelGroupName = a} :: GetOriginEndpointResponse)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
getOriginEndpointResponse_channelName :: Lens.Lens' GetOriginEndpointResponse Prelude.Text
getOriginEndpointResponse_channelName = Lens.lens (\GetOriginEndpointResponse' {channelName} -> channelName) (\s@GetOriginEndpointResponse' {} a -> s {channelName = a} :: GetOriginEndpointResponse)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
getOriginEndpointResponse_originEndpointName :: Lens.Lens' GetOriginEndpointResponse Prelude.Text
getOriginEndpointResponse_originEndpointName = Lens.lens (\GetOriginEndpointResponse' {originEndpointName} -> originEndpointName) (\s@GetOriginEndpointResponse' {} a -> s {originEndpointName = a} :: GetOriginEndpointResponse)

-- | The type of container attached to this origin endpoint.
getOriginEndpointResponse_containerType :: Lens.Lens' GetOriginEndpointResponse ContainerType
getOriginEndpointResponse_containerType = Lens.lens (\GetOriginEndpointResponse' {containerType} -> containerType) (\s@GetOriginEndpointResponse' {} a -> s {containerType = a} :: GetOriginEndpointResponse)

-- | Undocumented member.
getOriginEndpointResponse_segment :: Lens.Lens' GetOriginEndpointResponse Segment
getOriginEndpointResponse_segment = Lens.lens (\GetOriginEndpointResponse' {segment} -> segment) (\s@GetOriginEndpointResponse' {} a -> s {segment = a} :: GetOriginEndpointResponse)

-- | The date and time the origin endpoint was created.
getOriginEndpointResponse_createdAt :: Lens.Lens' GetOriginEndpointResponse Prelude.UTCTime
getOriginEndpointResponse_createdAt = Lens.lens (\GetOriginEndpointResponse' {createdAt} -> createdAt) (\s@GetOriginEndpointResponse' {} a -> s {createdAt = a} :: GetOriginEndpointResponse) Prelude.. Data._Time

-- | The date and time the origin endpoint was modified.
getOriginEndpointResponse_modifiedAt :: Lens.Lens' GetOriginEndpointResponse Prelude.UTCTime
getOriginEndpointResponse_modifiedAt = Lens.lens (\GetOriginEndpointResponse' {modifiedAt} -> modifiedAt) (\s@GetOriginEndpointResponse' {} a -> s {modifiedAt = a} :: GetOriginEndpointResponse) Prelude.. Data._Time

instance Prelude.NFData GetOriginEndpointResponse where
  rnf GetOriginEndpointResponse' {..} =
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
