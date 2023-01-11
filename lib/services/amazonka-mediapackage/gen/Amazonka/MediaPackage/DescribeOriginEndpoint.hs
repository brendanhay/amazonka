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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authorization")
            Prelude.<*> (x Data..?> "channelId")
            Prelude.<*> (x Data..?> "cmafPackage")
            Prelude.<*> (x Data..?> "dashPackage")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "hlsPackage")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "manifestName")
            Prelude.<*> (x Data..?> "mssPackage")
            Prelude.<*> (x Data..?> "origination")
            Prelude.<*> (x Data..?> "startoverWindowSeconds")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "timeDelaySeconds")
            Prelude.<*> (x Data..?> "url")
            Prelude.<*> (x Data..?> "whitelist" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOriginEndpoint where
  hashWithSalt _salt DescribeOriginEndpoint' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeOriginEndpoint where
  rnf DescribeOriginEndpoint' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeOriginEndpoint where
  toPath DescribeOriginEndpoint' {..} =
    Prelude.mconcat
      ["/origin_endpoints/", Data.toBS id]

instance Data.ToQuery DescribeOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Prelude.Maybe Prelude.Text,
    cmafPackage :: Prelude.Maybe CmafPackage,
    dashPackage :: Prelude.Maybe DashPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The ID of the OriginEndpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Prelude.Maybe Prelude.Text,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Prelude.Maybe Origination,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Prelude.Maybe Prelude.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
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
-- 'arn', 'describeOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'authorization', 'describeOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'channelId', 'describeOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'describeOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'dashPackage', 'describeOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'description', 'describeOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'hlsPackage', 'describeOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'id', 'describeOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'manifestName', 'describeOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'mssPackage', 'describeOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'origination', 'describeOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'startoverWindowSeconds', 'describeOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'tags', 'describeOriginEndpointResponse_tags' - Undocumented member.
--
-- 'timeDelaySeconds', 'describeOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'url', 'describeOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'whitelist', 'describeOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'httpStatus', 'describeOriginEndpointResponse_httpStatus' - The response's http status code.
newDescribeOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOriginEndpointResponse
newDescribeOriginEndpointResponse pHttpStatus_ =
  DescribeOriginEndpointResponse'
    { arn =
        Prelude.Nothing,
      authorization = Prelude.Nothing,
      channelId = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      description = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      id = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      origination = Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      url = Prelude.Nothing,
      whitelist = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
describeOriginEndpointResponse_arn :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_arn = Lens.lens (\DescribeOriginEndpointResponse' {arn} -> arn) (\s@DescribeOriginEndpointResponse' {} a -> s {arn = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_authorization :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Authorization)
describeOriginEndpointResponse_authorization = Lens.lens (\DescribeOriginEndpointResponse' {authorization} -> authorization) (\s@DescribeOriginEndpointResponse' {} a -> s {authorization = a} :: DescribeOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
describeOriginEndpointResponse_channelId :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_channelId = Lens.lens (\DescribeOriginEndpointResponse' {channelId} -> channelId) (\s@DescribeOriginEndpointResponse' {} a -> s {channelId = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_cmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe CmafPackage)
describeOriginEndpointResponse_cmafPackage = Lens.lens (\DescribeOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {cmafPackage = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_dashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe DashPackage)
describeOriginEndpointResponse_dashPackage = Lens.lens (\DescribeOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {dashPackage = a} :: DescribeOriginEndpointResponse)

-- | A short text description of the OriginEndpoint.
describeOriginEndpointResponse_description :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_description = Lens.lens (\DescribeOriginEndpointResponse' {description} -> description) (\s@DescribeOriginEndpointResponse' {} a -> s {description = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_hlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe HlsPackage)
describeOriginEndpointResponse_hlsPackage = Lens.lens (\DescribeOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {hlsPackage = a} :: DescribeOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
describeOriginEndpointResponse_id :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_id = Lens.lens (\DescribeOriginEndpointResponse' {id} -> id) (\s@DescribeOriginEndpointResponse' {} a -> s {id = a} :: DescribeOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
describeOriginEndpointResponse_manifestName :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_manifestName = Lens.lens (\DescribeOriginEndpointResponse' {manifestName} -> manifestName) (\s@DescribeOriginEndpointResponse' {} a -> s {manifestName = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_mssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe MssPackage)
describeOriginEndpointResponse_mssPackage = Lens.lens (\DescribeOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@DescribeOriginEndpointResponse' {} a -> s {mssPackage = a} :: DescribeOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
describeOriginEndpointResponse_origination :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Origination)
describeOriginEndpointResponse_origination = Lens.lens (\DescribeOriginEndpointResponse' {origination} -> origination) (\s@DescribeOriginEndpointResponse' {} a -> s {origination = a} :: DescribeOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
describeOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\DescribeOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: DescribeOriginEndpointResponse)

-- | Undocumented member.
describeOriginEndpointResponse_tags :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeOriginEndpointResponse_tags = Lens.lens (\DescribeOriginEndpointResponse' {tags} -> tags) (\s@DescribeOriginEndpointResponse' {} a -> s {tags = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
describeOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Int)
describeOriginEndpointResponse_timeDelaySeconds = Lens.lens (\DescribeOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@DescribeOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: DescribeOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
describeOriginEndpointResponse_url :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe Prelude.Text)
describeOriginEndpointResponse_url = Lens.lens (\DescribeOriginEndpointResponse' {url} -> url) (\s@DescribeOriginEndpointResponse' {} a -> s {url = a} :: DescribeOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
describeOriginEndpointResponse_whitelist :: Lens.Lens' DescribeOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
describeOriginEndpointResponse_whitelist = Lens.lens (\DescribeOriginEndpointResponse' {whitelist} -> whitelist) (\s@DescribeOriginEndpointResponse' {} a -> s {whitelist = a} :: DescribeOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeOriginEndpointResponse_httpStatus :: Lens.Lens' DescribeOriginEndpointResponse Prelude.Int
describeOriginEndpointResponse_httpStatus = Lens.lens (\DescribeOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeOriginEndpointResponse' {} a -> s {httpStatus = a} :: DescribeOriginEndpointResponse)

instance
  Prelude.NFData
    DescribeOriginEndpointResponse
  where
  rnf DescribeOriginEndpointResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf origination
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeDelaySeconds
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf whitelist
      `Prelude.seq` Prelude.rnf httpStatus
