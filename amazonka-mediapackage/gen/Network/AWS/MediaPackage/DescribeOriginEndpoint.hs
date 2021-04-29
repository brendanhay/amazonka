{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.DescribeOriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing OriginEndpoint.
module Network.AWS.MediaPackage.DescribeOriginEndpoint
  ( -- * Creating a Request
    DescribeOriginEndpoint (..),
    newDescribeOriginEndpoint,

    -- * Request Lenses
    describeOriginEndpoint_id,

    -- * Destructuring the Response
    DescribeOriginEndpointResponse (..),
    newDescribeOriginEndpointResponse,

    -- * Response Lenses
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOriginEndpoint' smart constructor.
data DescribeOriginEndpoint = DescribeOriginEndpoint'
  { -- | The ID of the OriginEndpoint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeOriginEndpoint_id' - The ID of the OriginEndpoint.
newDescribeOriginEndpoint ::
  -- | 'id'
  Prelude.Text ->
  DescribeOriginEndpoint
newDescribeOriginEndpoint pId_ =
  DescribeOriginEndpoint' {id = pId_}

-- | The ID of the OriginEndpoint.
describeOriginEndpoint_id :: Lens.Lens' DescribeOriginEndpoint Prelude.Text
describeOriginEndpoint_id = Lens.lens (\DescribeOriginEndpoint' {id} -> id) (\s@DescribeOriginEndpoint' {} a -> s {id = a} :: DescribeOriginEndpoint)

instance Prelude.AWSRequest DescribeOriginEndpoint where
  type
    Rs DescribeOriginEndpoint =
      DescribeOriginEndpointResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOriginEndpointResponse'
            Prelude.<$> (x Prelude..?> "dashPackage")
            Prelude.<*> (x Prelude..?> "startoverWindowSeconds")
            Prelude.<*> (x Prelude..?> "origination")
            Prelude.<*> (x Prelude..?> "channelId")
            Prelude.<*> (x Prelude..?> "cmafPackage")
            Prelude.<*> (x Prelude..?> "manifestName")
            Prelude.<*> (x Prelude..?> "arn")
            Prelude.<*> (x Prelude..?> "id")
            Prelude.<*> ( x Prelude..?> "whitelist"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "mssPackage")
            Prelude.<*> (x Prelude..?> "tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (x Prelude..?> "timeDelaySeconds")
            Prelude.<*> (x Prelude..?> "authorization")
            Prelude.<*> (x Prelude..?> "url")
            Prelude.<*> (x Prelude..?> "hlsPackage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOriginEndpoint

instance Prelude.NFData DescribeOriginEndpoint

instance Prelude.ToHeaders DescribeOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DescribeOriginEndpoint where
  toPath DescribeOriginEndpoint' {..} =
    Prelude.mconcat
      ["/origin_endpoints/", Prelude.toBS id]

instance Prelude.ToQuery DescribeOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
  { dashPackage :: Prelude.Maybe DashPackage,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Prelude.Maybe Origination,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Prelude.Maybe Prelude.Text,
    cmafPackage :: Prelude.Maybe CmafPackage,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OriginEndpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
    mssPackage :: Prelude.Maybe MssPackage,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    authorization :: Prelude.Maybe Authorization,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Prelude.Maybe Prelude.Text,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'describeOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'describeOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'describeOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'channelId', 'describeOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'describeOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'describeOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'arn', 'describeOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'id', 'describeOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'whitelist', 'describeOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'describeOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'tags', 'describeOriginEndpointResponse_tags' - Undocumented member.
--
-- 'description', 'describeOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'describeOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'describeOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'url', 'describeOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'hlsPackage', 'describeOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'httpStatus', 'describeOriginEndpointResponse_httpStatus' - The response's http status code.
newDescribeOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOriginEndpointResponse
newDescribeOriginEndpointResponse pHttpStatus_ =
  DescribeOriginEndpointResponse'
    { dashPackage =
        Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      origination = Prelude.Nothing,
      channelId = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      whitelist = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      authorization = Prelude.Nothing,
      url = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeOriginEndpointResponse_dashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe DashPackage)
describeOriginEndpointResponse_dashPackage = Lens.lens (\DescribeOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {dashPackage = a} :: DescribeOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
describeOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\DescribeOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: DescribeOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
describeOriginEndpointResponse_origination :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Origination)
describeOriginEndpointResponse_origination = Lens.lens (\DescribeOriginEndpointResponse' {origination} -> origination) (\s@DescribeOriginEndpointResponse' {} a -> s {origination = a} :: DescribeOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
describeOriginEndpointResponse_channelId :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_channelId = Lens.lens (\DescribeOriginEndpointResponse' {channelId} -> channelId) (\s@DescribeOriginEndpointResponse' {} a -> s {channelId = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_cmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe CmafPackage)
describeOriginEndpointResponse_cmafPackage = Lens.lens (\DescribeOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {cmafPackage = a} :: DescribeOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
describeOriginEndpointResponse_manifestName :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_manifestName = Lens.lens (\DescribeOriginEndpointResponse' {manifestName} -> manifestName) (\s@DescribeOriginEndpointResponse' {} a -> s {manifestName = a} :: DescribeOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
describeOriginEndpointResponse_arn :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_arn = Lens.lens (\DescribeOriginEndpointResponse' {arn} -> arn) (\s@DescribeOriginEndpointResponse' {} a -> s {arn = a} :: DescribeOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
describeOriginEndpointResponse_id :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_id = Lens.lens (\DescribeOriginEndpointResponse' {id} -> id) (\s@DescribeOriginEndpointResponse' {} a -> s {id = a} :: DescribeOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
describeOriginEndpointResponse_whitelist :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
describeOriginEndpointResponse_whitelist = Lens.lens (\DescribeOriginEndpointResponse' {whitelist} -> whitelist) (\s@DescribeOriginEndpointResponse' {} a -> s {whitelist = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
describeOriginEndpointResponse_mssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe MssPackage)
describeOriginEndpointResponse_mssPackage = Lens.lens (\DescribeOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {mssPackage = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_tags :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeOriginEndpointResponse_tags = Lens.lens (\DescribeOriginEndpointResponse' {tags} -> tags) (\s@DescribeOriginEndpointResponse' {} a -> s {tags = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A short text description of the OriginEndpoint.
describeOriginEndpointResponse_description :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_description = Lens.lens (\DescribeOriginEndpointResponse' {description} -> description) (\s@DescribeOriginEndpointResponse' {} a -> s {description = a} :: DescribeOriginEndpointResponse)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
describeOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_timeDelaySeconds = Lens.lens (\DescribeOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_authorization :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Authorization)
describeOriginEndpointResponse_authorization = Lens.lens (\DescribeOriginEndpointResponse' {authorization} -> authorization) (\s@DescribeOriginEndpointResponse' {} a -> s {authorization = a} :: DescribeOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
describeOriginEndpointResponse_url :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_url = Lens.lens (\DescribeOriginEndpointResponse' {url} -> url) (\s@DescribeOriginEndpointResponse' {} a -> s {url = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_hlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe HlsPackage)
describeOriginEndpointResponse_hlsPackage = Lens.lens (\DescribeOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {hlsPackage = a} :: DescribeOriginEndpointResponse)

-- | The response's http status code.
describeOriginEndpointResponse_httpStatus :: Lens.Lens' DescribeOriginEndpointResponse Prelude.Int
describeOriginEndpointResponse_httpStatus = Lens.lens (\DescribeOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeOriginEndpointResponse' {} a -> s {httpStatus = a} :: DescribeOriginEndpointResponse)

instance
  Prelude.NFData
    DescribeOriginEndpointResponse
