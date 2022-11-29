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
-- Module      : Amazonka.MediaPackage.DescribeOriginEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing OriginEndpoint.
module Amazonka.MediaPackage.DescribeOriginEndpoint
  ( -- * Creating a Request
    DescribeOriginEndpoint (..),
    newDescribeOriginEndpoint,

    -- * Request Lenses
    describeOriginEndpoint_id,

    -- * Destructuring the Response
    DescribeOriginEndpointResponse (..),
    newDescribeOriginEndpointResponse,

    -- * Response Lenses
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOriginEndpoint' smart constructor.
data DescribeOriginEndpoint = DescribeOriginEndpoint'
  { -- | The ID of the OriginEndpoint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeOriginEndpoint where
  type
    AWSResponse DescribeOriginEndpoint =
      DescribeOriginEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOriginEndpointResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "timeDelaySeconds")
            Prelude.<*> (x Core..?> "startoverWindowSeconds")
            Prelude.<*> (x Core..?> "mssPackage")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "whitelist" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "url")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "manifestName")
            Prelude.<*> (x Core..?> "channelId")
            Prelude.<*> (x Core..?> "authorization")
            Prelude.<*> (x Core..?> "dashPackage")
            Prelude.<*> (x Core..?> "cmafPackage")
            Prelude.<*> (x Core..?> "hlsPackage")
            Prelude.<*> (x Core..?> "origination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOriginEndpoint where
  hashWithSalt _salt DescribeOriginEndpoint' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeOriginEndpoint where
  rnf DescribeOriginEndpoint' {..} = Prelude.rnf id

instance Core.ToHeaders DescribeOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeOriginEndpoint where
  toPath DescribeOriginEndpoint' {..} =
    Prelude.mconcat
      ["/origin_endpoints/", Core.toBS id]

instance Core.ToQuery DescribeOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Prelude.Maybe Prelude.Text,
    -- | The ID of the OriginEndpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    dashPackage :: Prelude.Maybe DashPackage,
    cmafPackage :: Prelude.Maybe CmafPackage,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Prelude.Maybe Origination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeOriginEndpointResponse_tags' - Undocumented member.
--
-- 'timeDelaySeconds', 'describeOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'startoverWindowSeconds', 'describeOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'mssPackage', 'describeOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'arn', 'describeOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'whitelist', 'describeOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'url', 'describeOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'id', 'describeOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'description', 'describeOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'manifestName', 'describeOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'channelId', 'describeOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'authorization', 'describeOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'dashPackage', 'describeOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'cmafPackage', 'describeOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'hlsPackage', 'describeOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'origination', 'describeOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'httpStatus', 'describeOriginEndpointResponse_httpStatus' - The response's http status code.
newDescribeOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOriginEndpointResponse
newDescribeOriginEndpointResponse pHttpStatus_ =
  DescribeOriginEndpointResponse'
    { tags =
        Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      arn = Prelude.Nothing,
      whitelist = Prelude.Nothing,
      url = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      channelId = Prelude.Nothing,
      authorization = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      origination = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeOriginEndpointResponse_tags :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeOriginEndpointResponse_tags = Lens.lens (\DescribeOriginEndpointResponse' {tags} -> tags) (\s@DescribeOriginEndpointResponse' {} a -> s {tags = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
describeOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_timeDelaySeconds = Lens.lens (\DescribeOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: DescribeOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
describeOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\DescribeOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_mssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe MssPackage)
describeOriginEndpointResponse_mssPackage = Lens.lens (\DescribeOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {mssPackage = a} :: DescribeOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
describeOriginEndpointResponse_arn :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_arn = Lens.lens (\DescribeOriginEndpointResponse' {arn} -> arn) (\s@DescribeOriginEndpointResponse' {} a -> s {arn = a} :: DescribeOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
describeOriginEndpointResponse_whitelist :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
describeOriginEndpointResponse_whitelist = Lens.lens (\DescribeOriginEndpointResponse' {whitelist} -> whitelist) (\s@DescribeOriginEndpointResponse' {} a -> s {whitelist = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the packaged OriginEndpoint for consumption.
describeOriginEndpointResponse_url :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_url = Lens.lens (\DescribeOriginEndpointResponse' {url} -> url) (\s@DescribeOriginEndpointResponse' {} a -> s {url = a} :: DescribeOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
describeOriginEndpointResponse_id :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_id = Lens.lens (\DescribeOriginEndpointResponse' {id} -> id) (\s@DescribeOriginEndpointResponse' {} a -> s {id = a} :: DescribeOriginEndpointResponse)

-- | A short text description of the OriginEndpoint.
describeOriginEndpointResponse_description :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_description = Lens.lens (\DescribeOriginEndpointResponse' {description} -> description) (\s@DescribeOriginEndpointResponse' {} a -> s {description = a} :: DescribeOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
describeOriginEndpointResponse_manifestName :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_manifestName = Lens.lens (\DescribeOriginEndpointResponse' {manifestName} -> manifestName) (\s@DescribeOriginEndpointResponse' {} a -> s {manifestName = a} :: DescribeOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
describeOriginEndpointResponse_channelId :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_channelId = Lens.lens (\DescribeOriginEndpointResponse' {channelId} -> channelId) (\s@DescribeOriginEndpointResponse' {} a -> s {channelId = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_authorization :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Authorization)
describeOriginEndpointResponse_authorization = Lens.lens (\DescribeOriginEndpointResponse' {authorization} -> authorization) (\s@DescribeOriginEndpointResponse' {} a -> s {authorization = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_dashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe DashPackage)
describeOriginEndpointResponse_dashPackage = Lens.lens (\DescribeOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {dashPackage = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_cmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe CmafPackage)
describeOriginEndpointResponse_cmafPackage = Lens.lens (\DescribeOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {cmafPackage = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_hlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe HlsPackage)
describeOriginEndpointResponse_hlsPackage = Lens.lens (\DescribeOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {hlsPackage = a} :: DescribeOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
describeOriginEndpointResponse_origination :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Origination)
describeOriginEndpointResponse_origination = Lens.lens (\DescribeOriginEndpointResponse' {origination} -> origination) (\s@DescribeOriginEndpointResponse' {} a -> s {origination = a} :: DescribeOriginEndpointResponse)

-- | The response's http status code.
describeOriginEndpointResponse_httpStatus :: Lens.Lens' DescribeOriginEndpointResponse Prelude.Int
describeOriginEndpointResponse_httpStatus = Lens.lens (\DescribeOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeOriginEndpointResponse' {} a -> s {httpStatus = a} :: DescribeOriginEndpointResponse)

instance
  Prelude.NFData
    DescribeOriginEndpointResponse
  where
  rnf DescribeOriginEndpointResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeDelaySeconds
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf whitelist
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf origination
      `Prelude.seq` Prelude.rnf httpStatus
