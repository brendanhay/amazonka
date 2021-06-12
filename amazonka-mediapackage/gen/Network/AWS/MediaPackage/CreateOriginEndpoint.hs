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
-- Module      : Network.AWS.MediaPackage.CreateOriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OriginEndpoint record.
module Network.AWS.MediaPackage.CreateOriginEndpoint
  ( -- * Creating a Request
    CreateOriginEndpoint (..),
    newCreateOriginEndpoint,

    -- * Request Lenses
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_origination,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_whitelist,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_tags,
    createOriginEndpoint_description,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_authorization,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,

    -- * Destructuring the Response
    CreateOriginEndpointResponse (..),
    newCreateOriginEndpointResponse,

    -- * Response Lenses
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'newCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { dashPackage :: Core.Maybe DashPackage,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Core.Maybe Origination,
    cmafPackage :: Core.Maybe CmafPackageCreateOrUpdateParameters,
    -- | A short string that will be used as the filename of the OriginEndpoint
    -- URL (defaults to \"index\").
    manifestName :: Core.Maybe Core.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text],
    mssPackage :: Core.Maybe MssPackage,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    authorization :: Core.Maybe Authorization,
    hlsPackage :: Core.Maybe HlsPackage,
    -- | The ID of the Channel that the OriginEndpoint will be associated with.
    -- This cannot be changed after the OriginEndpoint is created.
    channelId :: Core.Text,
    -- | The ID of the OriginEndpoint. The ID must be unique within the region
    -- and it cannot be changed after the OriginEndpoint is created.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'createOriginEndpoint_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'createOriginEndpoint_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'createOriginEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'cmafPackage', 'createOriginEndpoint_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'createOriginEndpoint_manifestName' - A short string that will be used as the filename of the OriginEndpoint
-- URL (defaults to \"index\").
--
-- 'whitelist', 'createOriginEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'createOriginEndpoint_mssPackage' - Undocumented member.
--
-- 'tags', 'createOriginEndpoint_tags' - Undocumented member.
--
-- 'description', 'createOriginEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'createOriginEndpoint_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'createOriginEndpoint_authorization' - Undocumented member.
--
-- 'hlsPackage', 'createOriginEndpoint_hlsPackage' - Undocumented member.
--
-- 'channelId', 'createOriginEndpoint_channelId' - The ID of the Channel that the OriginEndpoint will be associated with.
-- This cannot be changed after the OriginEndpoint is created.
--
-- 'id', 'createOriginEndpoint_id' - The ID of the OriginEndpoint. The ID must be unique within the region
-- and it cannot be changed after the OriginEndpoint is created.
newCreateOriginEndpoint ::
  -- | 'channelId'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  CreateOriginEndpoint
newCreateOriginEndpoint pChannelId_ pId_ =
  CreateOriginEndpoint'
    { dashPackage = Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      origination = Core.Nothing,
      cmafPackage = Core.Nothing,
      manifestName = Core.Nothing,
      whitelist = Core.Nothing,
      mssPackage = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      authorization = Core.Nothing,
      hlsPackage = Core.Nothing,
      channelId = pChannelId_,
      id = pId_
    }

-- | Undocumented member.
createOriginEndpoint_dashPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe DashPackage)
createOriginEndpoint_dashPackage = Lens.lens (\CreateOriginEndpoint' {dashPackage} -> dashPackage) (\s@CreateOriginEndpoint' {} a -> s {dashPackage = a} :: CreateOriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpoint_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
createOriginEndpoint_startoverWindowSeconds = Lens.lens (\CreateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpoint_origination :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Origination)
createOriginEndpoint_origination = Lens.lens (\CreateOriginEndpoint' {origination} -> origination) (\s@CreateOriginEndpoint' {} a -> s {origination = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_cmafPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe CmafPackageCreateOrUpdateParameters)
createOriginEndpoint_cmafPackage = Lens.lens (\CreateOriginEndpoint' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpoint' {} a -> s {cmafPackage = a} :: CreateOriginEndpoint)

-- | A short string that will be used as the filename of the OriginEndpoint
-- URL (defaults to \"index\").
createOriginEndpoint_manifestName :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
createOriginEndpoint_manifestName = Lens.lens (\CreateOriginEndpoint' {manifestName} -> manifestName) (\s@CreateOriginEndpoint' {} a -> s {manifestName = a} :: CreateOriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpoint_whitelist :: Lens.Lens' CreateOriginEndpoint (Core.Maybe [Core.Text])
createOriginEndpoint_whitelist = Lens.lens (\CreateOriginEndpoint' {whitelist} -> whitelist) (\s@CreateOriginEndpoint' {} a -> s {whitelist = a} :: CreateOriginEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createOriginEndpoint_mssPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe MssPackage)
createOriginEndpoint_mssPackage = Lens.lens (\CreateOriginEndpoint' {mssPackage} -> mssPackage) (\s@CreateOriginEndpoint' {} a -> s {mssPackage = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_tags :: Lens.Lens' CreateOriginEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
createOriginEndpoint_tags = Lens.lens (\CreateOriginEndpoint' {tags} -> tags) (\s@CreateOriginEndpoint' {} a -> s {tags = a} :: CreateOriginEndpoint) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the OriginEndpoint.
createOriginEndpoint_description :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
createOriginEndpoint_description = Lens.lens (\CreateOriginEndpoint' {description} -> description) (\s@CreateOriginEndpoint' {} a -> s {description = a} :: CreateOriginEndpoint)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpoint_timeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
createOriginEndpoint_timeDelaySeconds = Lens.lens (\CreateOriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpoint' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_authorization :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Authorization)
createOriginEndpoint_authorization = Lens.lens (\CreateOriginEndpoint' {authorization} -> authorization) (\s@CreateOriginEndpoint' {} a -> s {authorization = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_hlsPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe HlsPackage)
createOriginEndpoint_hlsPackage = Lens.lens (\CreateOriginEndpoint' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpoint' {} a -> s {hlsPackage = a} :: CreateOriginEndpoint)

-- | The ID of the Channel that the OriginEndpoint will be associated with.
-- This cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_channelId :: Lens.Lens' CreateOriginEndpoint Core.Text
createOriginEndpoint_channelId = Lens.lens (\CreateOriginEndpoint' {channelId} -> channelId) (\s@CreateOriginEndpoint' {} a -> s {channelId = a} :: CreateOriginEndpoint)

-- | The ID of the OriginEndpoint. The ID must be unique within the region
-- and it cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_id :: Lens.Lens' CreateOriginEndpoint Core.Text
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
            Core.<$> (x Core..?> "dashPackage")
            Core.<*> (x Core..?> "startoverWindowSeconds")
            Core.<*> (x Core..?> "origination")
            Core.<*> (x Core..?> "channelId")
            Core.<*> (x Core..?> "cmafPackage")
            Core.<*> (x Core..?> "manifestName")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "whitelist" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "mssPackage")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "timeDelaySeconds")
            Core.<*> (x Core..?> "authorization")
            Core.<*> (x Core..?> "url")
            Core.<*> (x Core..?> "hlsPackage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOriginEndpoint

instance Core.NFData CreateOriginEndpoint

instance Core.ToHeaders CreateOriginEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("dashPackage" Core..=) Core.<$> dashPackage,
            ("startoverWindowSeconds" Core..=)
              Core.<$> startoverWindowSeconds,
            ("origination" Core..=) Core.<$> origination,
            ("cmafPackage" Core..=) Core.<$> cmafPackage,
            ("manifestName" Core..=) Core.<$> manifestName,
            ("whitelist" Core..=) Core.<$> whitelist,
            ("mssPackage" Core..=) Core.<$> mssPackage,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("timeDelaySeconds" Core..=)
              Core.<$> timeDelaySeconds,
            ("authorization" Core..=) Core.<$> authorization,
            ("hlsPackage" Core..=) Core.<$> hlsPackage,
            Core.Just ("channelId" Core..= channelId),
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath CreateOriginEndpoint where
  toPath = Core.const "/origin_endpoints"

instance Core.ToQuery CreateOriginEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
  { dashPackage :: Core.Maybe DashPackage,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    -- If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Core.Maybe Origination,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Core.Maybe Core.Text,
    cmafPackage :: Core.Maybe CmafPackage,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the OriginEndpoint.
    id :: Core.Maybe Core.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text],
    mssPackage :: Core.Maybe MssPackage,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content. If
    -- not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    authorization :: Core.Maybe Authorization,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Core.Maybe Core.Text,
    hlsPackage :: Core.Maybe HlsPackage,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'createOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'createOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'createOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'channelId', 'createOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'createOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'createOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'arn', 'createOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'id', 'createOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'whitelist', 'createOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'createOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'tags', 'createOriginEndpointResponse_tags' - Undocumented member.
--
-- 'description', 'createOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'createOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'createOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'url', 'createOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'hlsPackage', 'createOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'httpStatus', 'createOriginEndpointResponse_httpStatus' - The response's http status code.
newCreateOriginEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOriginEndpointResponse
newCreateOriginEndpointResponse pHttpStatus_ =
  CreateOriginEndpointResponse'
    { dashPackage =
        Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      origination = Core.Nothing,
      channelId = Core.Nothing,
      cmafPackage = Core.Nothing,
      manifestName = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      whitelist = Core.Nothing,
      mssPackage = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      authorization = Core.Nothing,
      url = Core.Nothing,
      hlsPackage = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createOriginEndpointResponse_dashPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe DashPackage)
createOriginEndpointResponse_dashPackage = Lens.lens (\CreateOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@CreateOriginEndpointResponse' {} a -> s {dashPackage = a} :: CreateOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
createOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\CreateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpointResponse_origination :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Origination)
createOriginEndpointResponse_origination = Lens.lens (\CreateOriginEndpointResponse' {origination} -> origination) (\s@CreateOriginEndpointResponse' {} a -> s {origination = a} :: CreateOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
createOriginEndpointResponse_channelId :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_channelId = Lens.lens (\CreateOriginEndpointResponse' {channelId} -> channelId) (\s@CreateOriginEndpointResponse' {} a -> s {channelId = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_cmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe CmafPackage)
createOriginEndpointResponse_cmafPackage = Lens.lens (\CreateOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpointResponse' {} a -> s {cmafPackage = a} :: CreateOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
createOriginEndpointResponse_manifestName :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_manifestName = Lens.lens (\CreateOriginEndpointResponse' {manifestName} -> manifestName) (\s@CreateOriginEndpointResponse' {} a -> s {manifestName = a} :: CreateOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
createOriginEndpointResponse_arn :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_arn = Lens.lens (\CreateOriginEndpointResponse' {arn} -> arn) (\s@CreateOriginEndpointResponse' {} a -> s {arn = a} :: CreateOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
createOriginEndpointResponse_id :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_id = Lens.lens (\CreateOriginEndpointResponse' {id} -> id) (\s@CreateOriginEndpointResponse' {} a -> s {id = a} :: CreateOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpointResponse_whitelist :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe [Core.Text])
createOriginEndpointResponse_whitelist = Lens.lens (\CreateOriginEndpointResponse' {whitelist} -> whitelist) (\s@CreateOriginEndpointResponse' {} a -> s {whitelist = a} :: CreateOriginEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createOriginEndpointResponse_mssPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe MssPackage)
createOriginEndpointResponse_mssPackage = Lens.lens (\CreateOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@CreateOriginEndpointResponse' {} a -> s {mssPackage = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_tags :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createOriginEndpointResponse_tags = Lens.lens (\CreateOriginEndpointResponse' {tags} -> tags) (\s@CreateOriginEndpointResponse' {} a -> s {tags = a} :: CreateOriginEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the OriginEndpoint.
createOriginEndpointResponse_description :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_description = Lens.lens (\CreateOriginEndpointResponse' {description} -> description) (\s@CreateOriginEndpointResponse' {} a -> s {description = a} :: CreateOriginEndpointResponse)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
createOriginEndpointResponse_timeDelaySeconds = Lens.lens (\CreateOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_authorization :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Authorization)
createOriginEndpointResponse_authorization = Lens.lens (\CreateOriginEndpointResponse' {authorization} -> authorization) (\s@CreateOriginEndpointResponse' {} a -> s {authorization = a} :: CreateOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
createOriginEndpointResponse_url :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
createOriginEndpointResponse_url = Lens.lens (\CreateOriginEndpointResponse' {url} -> url) (\s@CreateOriginEndpointResponse' {} a -> s {url = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_hlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe HlsPackage)
createOriginEndpointResponse_hlsPackage = Lens.lens (\CreateOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpointResponse' {} a -> s {hlsPackage = a} :: CreateOriginEndpointResponse)

-- | The response's http status code.
createOriginEndpointResponse_httpStatus :: Lens.Lens' CreateOriginEndpointResponse Core.Int
createOriginEndpointResponse_httpStatus = Lens.lens (\CreateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateOriginEndpointResponse' {} a -> s {httpStatus = a} :: CreateOriginEndpointResponse)

instance Core.NFData CreateOriginEndpointResponse
