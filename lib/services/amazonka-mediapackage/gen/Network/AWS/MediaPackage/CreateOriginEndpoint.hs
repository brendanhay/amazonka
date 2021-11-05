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
-- Module      : Amazonka.MediaPackage.CreateOriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OriginEndpoint record.
module Amazonka.MediaPackage.CreateOriginEndpoint
  ( -- * Creating a Request
    CreateOriginEndpoint (..),
    newCreateOriginEndpoint,

    -- * Request Lenses
    createOriginEndpoint_whitelist,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_authorization,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_description,
    createOriginEndpoint_tags,
    createOriginEndpoint_origination,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,

    -- * Destructuring the Response
    CreateOriginEndpointResponse (..),
    newCreateOriginEndpointResponse,

    -- * Response Lenses
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'newCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | A short string that will be used as the filename of the OriginEndpoint
    -- URL (defaults to \"index\").
    manifestName :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    dashPackage :: Prelude.Maybe DashPackage,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    cmafPackage :: Prelude.Maybe CmafPackageCreateOrUpdateParameters,
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Prelude.Maybe Origination,
    -- | The ID of the Channel that the OriginEndpoint will be associated with.
    -- This cannot be changed after the OriginEndpoint is created.
    channelId :: Prelude.Text,
    -- | The ID of the OriginEndpoint. The ID must be unique within the region
    -- and it cannot be changed after the OriginEndpoint is created.
    id :: Prelude.Text
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
-- 'whitelist', 'createOriginEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'hlsPackage', 'createOriginEndpoint_hlsPackage' - Undocumented member.
--
-- 'manifestName', 'createOriginEndpoint_manifestName' - A short string that will be used as the filename of the OriginEndpoint
-- URL (defaults to \"index\").
--
-- 'authorization', 'createOriginEndpoint_authorization' - Undocumented member.
--
-- 'startoverWindowSeconds', 'createOriginEndpoint_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'dashPackage', 'createOriginEndpoint_dashPackage' - Undocumented member.
--
-- 'mssPackage', 'createOriginEndpoint_mssPackage' - Undocumented member.
--
-- 'timeDelaySeconds', 'createOriginEndpoint_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'cmafPackage', 'createOriginEndpoint_cmafPackage' - Undocumented member.
--
-- 'description', 'createOriginEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'tags', 'createOriginEndpoint_tags' - Undocumented member.
--
-- 'origination', 'createOriginEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'channelId', 'createOriginEndpoint_channelId' - The ID of the Channel that the OriginEndpoint will be associated with.
-- This cannot be changed after the OriginEndpoint is created.
--
-- 'id', 'createOriginEndpoint_id' - The ID of the OriginEndpoint. The ID must be unique within the region
-- and it cannot be changed after the OriginEndpoint is created.
newCreateOriginEndpoint ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  CreateOriginEndpoint
newCreateOriginEndpoint pChannelId_ pId_ =
  CreateOriginEndpoint'
    { whitelist = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      authorization = Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      origination = Prelude.Nothing,
      channelId = pChannelId_,
      id = pId_
    }

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpoint_whitelist :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe [Prelude.Text])
createOriginEndpoint_whitelist = Lens.lens (\CreateOriginEndpoint' {whitelist} -> whitelist) (\s@CreateOriginEndpoint' {} a -> s {whitelist = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createOriginEndpoint_hlsPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe HlsPackage)
createOriginEndpoint_hlsPackage = Lens.lens (\CreateOriginEndpoint' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpoint' {} a -> s {hlsPackage = a} :: CreateOriginEndpoint)

-- | A short string that will be used as the filename of the OriginEndpoint
-- URL (defaults to \"index\").
createOriginEndpoint_manifestName :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_manifestName = Lens.lens (\CreateOriginEndpoint' {manifestName} -> manifestName) (\s@CreateOriginEndpoint' {} a -> s {manifestName = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_authorization :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Authorization)
createOriginEndpoint_authorization = Lens.lens (\CreateOriginEndpoint' {authorization} -> authorization) (\s@CreateOriginEndpoint' {} a -> s {authorization = a} :: CreateOriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpoint_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Int)
createOriginEndpoint_startoverWindowSeconds = Lens.lens (\CreateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_dashPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe DashPackage)
createOriginEndpoint_dashPackage = Lens.lens (\CreateOriginEndpoint' {dashPackage} -> dashPackage) (\s@CreateOriginEndpoint' {} a -> s {dashPackage = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_mssPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe MssPackage)
createOriginEndpoint_mssPackage = Lens.lens (\CreateOriginEndpoint' {mssPackage} -> mssPackage) (\s@CreateOriginEndpoint' {} a -> s {mssPackage = a} :: CreateOriginEndpoint)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpoint_timeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Int)
createOriginEndpoint_timeDelaySeconds = Lens.lens (\CreateOriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpoint' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_cmafPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe CmafPackageCreateOrUpdateParameters)
createOriginEndpoint_cmafPackage = Lens.lens (\CreateOriginEndpoint' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpoint' {} a -> s {cmafPackage = a} :: CreateOriginEndpoint)

-- | A short text description of the OriginEndpoint.
createOriginEndpoint_description :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_description = Lens.lens (\CreateOriginEndpoint' {description} -> description) (\s@CreateOriginEndpoint' {} a -> s {description = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_tags :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpoint_tags = Lens.lens (\CreateOriginEndpoint' {tags} -> tags) (\s@CreateOriginEndpoint' {} a -> s {tags = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpoint_origination :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Origination)
createOriginEndpoint_origination = Lens.lens (\CreateOriginEndpoint' {origination} -> origination) (\s@CreateOriginEndpoint' {} a -> s {origination = a} :: CreateOriginEndpoint)

-- | The ID of the Channel that the OriginEndpoint will be associated with.
-- This cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_channelId :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_channelId = Lens.lens (\CreateOriginEndpoint' {channelId} -> channelId) (\s@CreateOriginEndpoint' {} a -> s {channelId = a} :: CreateOriginEndpoint)

-- | The ID of the OriginEndpoint. The ID must be unique within the region
-- and it cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_id :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_id = Lens.lens (\CreateOriginEndpoint' {id} -> id) (\s@CreateOriginEndpoint' {} a -> s {id = a} :: CreateOriginEndpoint)

instance Core.AWSRequest CreateOriginEndpoint where
  type
    AWSResponse CreateOriginEndpoint =
      CreateOriginEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOriginEndpointResponse'
            Prelude.<$> (x Core..?> "whitelist" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "hlsPackage")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "manifestName")
            Prelude.<*> (x Core..?> "url")
            Prelude.<*> (x Core..?> "authorization")
            Prelude.<*> (x Core..?> "channelId")
            Prelude.<*> (x Core..?> "startoverWindowSeconds")
            Prelude.<*> (x Core..?> "dashPackage")
            Prelude.<*> (x Core..?> "mssPackage")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "timeDelaySeconds")
            Prelude.<*> (x Core..?> "cmafPackage")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "origination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOriginEndpoint

instance Prelude.NFData CreateOriginEndpoint

instance Core.ToHeaders CreateOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("whitelist" Core..=) Prelude.<$> whitelist,
            ("hlsPackage" Core..=) Prelude.<$> hlsPackage,
            ("manifestName" Core..=) Prelude.<$> manifestName,
            ("authorization" Core..=) Prelude.<$> authorization,
            ("startoverWindowSeconds" Core..=)
              Prelude.<$> startoverWindowSeconds,
            ("dashPackage" Core..=) Prelude.<$> dashPackage,
            ("mssPackage" Core..=) Prelude.<$> mssPackage,
            ("timeDelaySeconds" Core..=)
              Prelude.<$> timeDelaySeconds,
            ("cmafPackage" Core..=) Prelude.<$> cmafPackage,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            ("origination" Core..=) Prelude.<$> origination,
            Prelude.Just ("channelId" Core..= channelId),
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath CreateOriginEndpoint where
  toPath = Prelude.const "/origin_endpoints"

instance Core.ToQuery CreateOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
  { -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Prelude.Maybe Prelude.Text,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    dashPackage :: Prelude.Maybe DashPackage,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | The ID of the OriginEndpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    cmafPackage :: Prelude.Maybe CmafPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- Create a value of 'CreateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whitelist', 'createOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'hlsPackage', 'createOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'arn', 'createOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'manifestName', 'createOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'url', 'createOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'authorization', 'createOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'channelId', 'createOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'startoverWindowSeconds', 'createOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'dashPackage', 'createOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'mssPackage', 'createOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'id', 'createOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'createOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'cmafPackage', 'createOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'description', 'createOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'tags', 'createOriginEndpointResponse_tags' - Undocumented member.
--
-- 'origination', 'createOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'httpStatus', 'createOriginEndpointResponse_httpStatus' - The response's http status code.
newCreateOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOriginEndpointResponse
newCreateOriginEndpointResponse pHttpStatus_ =
  CreateOriginEndpointResponse'
    { whitelist =
        Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      arn = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      url = Prelude.Nothing,
      authorization = Prelude.Nothing,
      channelId = Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      id = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      origination = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpointResponse_whitelist :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
createOriginEndpointResponse_whitelist = Lens.lens (\CreateOriginEndpointResponse' {whitelist} -> whitelist) (\s@CreateOriginEndpointResponse' {} a -> s {whitelist = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createOriginEndpointResponse_hlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe HlsPackage)
createOriginEndpointResponse_hlsPackage = Lens.lens (\CreateOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpointResponse' {} a -> s {hlsPackage = a} :: CreateOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
createOriginEndpointResponse_arn :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_arn = Lens.lens (\CreateOriginEndpointResponse' {arn} -> arn) (\s@CreateOriginEndpointResponse' {} a -> s {arn = a} :: CreateOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
createOriginEndpointResponse_manifestName :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_manifestName = Lens.lens (\CreateOriginEndpointResponse' {manifestName} -> manifestName) (\s@CreateOriginEndpointResponse' {} a -> s {manifestName = a} :: CreateOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
createOriginEndpointResponse_url :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_url = Lens.lens (\CreateOriginEndpointResponse' {url} -> url) (\s@CreateOriginEndpointResponse' {} a -> s {url = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_authorization :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Authorization)
createOriginEndpointResponse_authorization = Lens.lens (\CreateOriginEndpointResponse' {authorization} -> authorization) (\s@CreateOriginEndpointResponse' {} a -> s {authorization = a} :: CreateOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
createOriginEndpointResponse_channelId :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_channelId = Lens.lens (\CreateOriginEndpointResponse' {channelId} -> channelId) (\s@CreateOriginEndpointResponse' {} a -> s {channelId = a} :: CreateOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
createOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\CreateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_dashPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe DashPackage)
createOriginEndpointResponse_dashPackage = Lens.lens (\CreateOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@CreateOriginEndpointResponse' {} a -> s {dashPackage = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_mssPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe MssPackage)
createOriginEndpointResponse_mssPackage = Lens.lens (\CreateOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@CreateOriginEndpointResponse' {} a -> s {mssPackage = a} :: CreateOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
createOriginEndpointResponse_id :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_id = Lens.lens (\CreateOriginEndpointResponse' {id} -> id) (\s@CreateOriginEndpointResponse' {} a -> s {id = a} :: CreateOriginEndpointResponse)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
createOriginEndpointResponse_timeDelaySeconds = Lens.lens (\CreateOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_cmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe CmafPackage)
createOriginEndpointResponse_cmafPackage = Lens.lens (\CreateOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpointResponse' {} a -> s {cmafPackage = a} :: CreateOriginEndpointResponse)

-- | A short text description of the OriginEndpoint.
createOriginEndpointResponse_description :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_description = Lens.lens (\CreateOriginEndpointResponse' {description} -> description) (\s@CreateOriginEndpointResponse' {} a -> s {description = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_tags :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpointResponse_tags = Lens.lens (\CreateOriginEndpointResponse' {tags} -> tags) (\s@CreateOriginEndpointResponse' {} a -> s {tags = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpointResponse_origination :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Origination)
createOriginEndpointResponse_origination = Lens.lens (\CreateOriginEndpointResponse' {origination} -> origination) (\s@CreateOriginEndpointResponse' {} a -> s {origination = a} :: CreateOriginEndpointResponse)

-- | The response's http status code.
createOriginEndpointResponse_httpStatus :: Lens.Lens' CreateOriginEndpointResponse Prelude.Int
createOriginEndpointResponse_httpStatus = Lens.lens (\CreateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateOriginEndpointResponse' {} a -> s {httpStatus = a} :: CreateOriginEndpointResponse)

instance Prelude.NFData CreateOriginEndpointResponse
