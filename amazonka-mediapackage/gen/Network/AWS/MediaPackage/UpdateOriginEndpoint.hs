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
-- Module      : Network.AWS.MediaPackage.UpdateOriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing OriginEndpoint.
module Network.AWS.MediaPackage.UpdateOriginEndpoint
  ( -- * Creating a Request
    UpdateOriginEndpoint (..),
    newUpdateOriginEndpoint,

    -- * Request Lenses
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_description,
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_id,

    -- * Destructuring the Response
    UpdateOriginEndpointResponse (..),
    newUpdateOriginEndpointResponse,

    -- * Response Lenses
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to update an existing OriginEndpoint.
--
-- /See:/ 'newUpdateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { dashPackage :: Core.Maybe DashPackage,
    -- | Maximum duration (in seconds) of content to retain for startover
    -- playback. If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Core.Maybe Origination,
    cmafPackage :: Core.Maybe CmafPackageCreateOrUpdateParameters,
    -- | A short string that will be appended to the end of the Endpoint URL.
    manifestName :: Core.Maybe Core.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text],
    mssPackage :: Core.Maybe MssPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    -- | Amount of delay (in seconds) to enforce on the playback of live content.
    -- If not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    authorization :: Core.Maybe Authorization,
    hlsPackage :: Core.Maybe HlsPackage,
    -- | The ID of the OriginEndpoint to update.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'updateOriginEndpoint_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'updateOriginEndpoint_startoverWindowSeconds' - Maximum duration (in seconds) of content to retain for startover
-- playback. If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'updateOriginEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'cmafPackage', 'updateOriginEndpoint_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'updateOriginEndpoint_manifestName' - A short string that will be appended to the end of the Endpoint URL.
--
-- 'whitelist', 'updateOriginEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'updateOriginEndpoint_mssPackage' - Undocumented member.
--
-- 'description', 'updateOriginEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'updateOriginEndpoint_timeDelaySeconds' - Amount of delay (in seconds) to enforce on the playback of live content.
-- If not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'updateOriginEndpoint_authorization' - Undocumented member.
--
-- 'hlsPackage', 'updateOriginEndpoint_hlsPackage' - Undocumented member.
--
-- 'id', 'updateOriginEndpoint_id' - The ID of the OriginEndpoint to update.
newUpdateOriginEndpoint ::
  -- | 'id'
  Core.Text ->
  UpdateOriginEndpoint
newUpdateOriginEndpoint pId_ =
  UpdateOriginEndpoint'
    { dashPackage = Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      origination = Core.Nothing,
      cmafPackage = Core.Nothing,
      manifestName = Core.Nothing,
      whitelist = Core.Nothing,
      mssPackage = Core.Nothing,
      description = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      authorization = Core.Nothing,
      hlsPackage = Core.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateOriginEndpoint_dashPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe DashPackage)
updateOriginEndpoint_dashPackage = Lens.lens (\UpdateOriginEndpoint' {dashPackage} -> dashPackage) (\s@UpdateOriginEndpoint' {} a -> s {dashPackage = a} :: UpdateOriginEndpoint)

-- | Maximum duration (in seconds) of content to retain for startover
-- playback. If not specified, startover playback will be disabled for the
-- OriginEndpoint.
updateOriginEndpoint_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Int)
updateOriginEndpoint_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
updateOriginEndpoint_origination :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Origination)
updateOriginEndpoint_origination = Lens.lens (\UpdateOriginEndpoint' {origination} -> origination) (\s@UpdateOriginEndpoint' {} a -> s {origination = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_cmafPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe CmafPackageCreateOrUpdateParameters)
updateOriginEndpoint_cmafPackage = Lens.lens (\UpdateOriginEndpoint' {cmafPackage} -> cmafPackage) (\s@UpdateOriginEndpoint' {} a -> s {cmafPackage = a} :: UpdateOriginEndpoint)

-- | A short string that will be appended to the end of the Endpoint URL.
updateOriginEndpoint_manifestName :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Text)
updateOriginEndpoint_manifestName = Lens.lens (\UpdateOriginEndpoint' {manifestName} -> manifestName) (\s@UpdateOriginEndpoint' {} a -> s {manifestName = a} :: UpdateOriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
updateOriginEndpoint_whitelist :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe [Core.Text])
updateOriginEndpoint_whitelist = Lens.lens (\UpdateOriginEndpoint' {whitelist} -> whitelist) (\s@UpdateOriginEndpoint' {} a -> s {whitelist = a} :: UpdateOriginEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
updateOriginEndpoint_mssPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe MssPackage)
updateOriginEndpoint_mssPackage = Lens.lens (\UpdateOriginEndpoint' {mssPackage} -> mssPackage) (\s@UpdateOriginEndpoint' {} a -> s {mssPackage = a} :: UpdateOriginEndpoint)

-- | A short text description of the OriginEndpoint.
updateOriginEndpoint_description :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Text)
updateOriginEndpoint_description = Lens.lens (\UpdateOriginEndpoint' {description} -> description) (\s@UpdateOriginEndpoint' {} a -> s {description = a} :: UpdateOriginEndpoint)

-- | Amount of delay (in seconds) to enforce on the playback of live content.
-- If not specified, there will be no time delay in effect for the
-- OriginEndpoint.
updateOriginEndpoint_timeDelaySeconds :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Int)
updateOriginEndpoint_timeDelaySeconds = Lens.lens (\UpdateOriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@UpdateOriginEndpoint' {} a -> s {timeDelaySeconds = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_authorization :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Authorization)
updateOriginEndpoint_authorization = Lens.lens (\UpdateOriginEndpoint' {authorization} -> authorization) (\s@UpdateOriginEndpoint' {} a -> s {authorization = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_hlsPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe HlsPackage)
updateOriginEndpoint_hlsPackage = Lens.lens (\UpdateOriginEndpoint' {hlsPackage} -> hlsPackage) (\s@UpdateOriginEndpoint' {} a -> s {hlsPackage = a} :: UpdateOriginEndpoint)

-- | The ID of the OriginEndpoint to update.
updateOriginEndpoint_id :: Lens.Lens' UpdateOriginEndpoint Core.Text
updateOriginEndpoint_id = Lens.lens (\UpdateOriginEndpoint' {id} -> id) (\s@UpdateOriginEndpoint' {} a -> s {id = a} :: UpdateOriginEndpoint)

instance Core.AWSRequest UpdateOriginEndpoint where
  type
    AWSResponse UpdateOriginEndpoint =
      UpdateOriginEndpointResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOriginEndpointResponse'
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

instance Core.Hashable UpdateOriginEndpoint

instance Core.NFData UpdateOriginEndpoint

instance Core.ToHeaders UpdateOriginEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateOriginEndpoint where
  toJSON UpdateOriginEndpoint' {..} =
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
            ("description" Core..=) Core.<$> description,
            ("timeDelaySeconds" Core..=)
              Core.<$> timeDelaySeconds,
            ("authorization" Core..=) Core.<$> authorization,
            ("hlsPackage" Core..=) Core.<$> hlsPackage
          ]
      )

instance Core.ToPath UpdateOriginEndpoint where
  toPath UpdateOriginEndpoint' {..} =
    Core.mconcat ["/origin_endpoints/", Core.toBS id]

instance Core.ToQuery UpdateOriginEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
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
-- Create a value of 'UpdateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'updateOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'updateOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'updateOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'channelId', 'updateOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'updateOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'updateOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'arn', 'updateOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'id', 'updateOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'whitelist', 'updateOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'updateOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'tags', 'updateOriginEndpointResponse_tags' - Undocumented member.
--
-- 'description', 'updateOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'updateOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'updateOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'url', 'updateOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'hlsPackage', 'updateOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'httpStatus', 'updateOriginEndpointResponse_httpStatus' - The response's http status code.
newUpdateOriginEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateOriginEndpointResponse
newUpdateOriginEndpointResponse pHttpStatus_ =
  UpdateOriginEndpointResponse'
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
updateOriginEndpointResponse_dashPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe DashPackage)
updateOriginEndpointResponse_dashPackage = Lens.lens (\UpdateOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {dashPackage = a} :: UpdateOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
updateOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Int)
updateOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
updateOriginEndpointResponse_origination :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Origination)
updateOriginEndpointResponse_origination = Lens.lens (\UpdateOriginEndpointResponse' {origination} -> origination) (\s@UpdateOriginEndpointResponse' {} a -> s {origination = a} :: UpdateOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
updateOriginEndpointResponse_channelId :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_channelId = Lens.lens (\UpdateOriginEndpointResponse' {channelId} -> channelId) (\s@UpdateOriginEndpointResponse' {} a -> s {channelId = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_cmafPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe CmafPackage)
updateOriginEndpointResponse_cmafPackage = Lens.lens (\UpdateOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {cmafPackage = a} :: UpdateOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
updateOriginEndpointResponse_manifestName :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_manifestName = Lens.lens (\UpdateOriginEndpointResponse' {manifestName} -> manifestName) (\s@UpdateOriginEndpointResponse' {} a -> s {manifestName = a} :: UpdateOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
updateOriginEndpointResponse_arn :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_arn = Lens.lens (\UpdateOriginEndpointResponse' {arn} -> arn) (\s@UpdateOriginEndpointResponse' {} a -> s {arn = a} :: UpdateOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
updateOriginEndpointResponse_id :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_id = Lens.lens (\UpdateOriginEndpointResponse' {id} -> id) (\s@UpdateOriginEndpointResponse' {} a -> s {id = a} :: UpdateOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
updateOriginEndpointResponse_whitelist :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe [Core.Text])
updateOriginEndpointResponse_whitelist = Lens.lens (\UpdateOriginEndpointResponse' {whitelist} -> whitelist) (\s@UpdateOriginEndpointResponse' {} a -> s {whitelist = a} :: UpdateOriginEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
updateOriginEndpointResponse_mssPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe MssPackage)
updateOriginEndpointResponse_mssPackage = Lens.lens (\UpdateOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {mssPackage = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_tags :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateOriginEndpointResponse_tags = Lens.lens (\UpdateOriginEndpointResponse' {tags} -> tags) (\s@UpdateOriginEndpointResponse' {} a -> s {tags = a} :: UpdateOriginEndpointResponse) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the OriginEndpoint.
updateOriginEndpointResponse_description :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_description = Lens.lens (\UpdateOriginEndpointResponse' {description} -> description) (\s@UpdateOriginEndpointResponse' {} a -> s {description = a} :: UpdateOriginEndpointResponse)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
updateOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Int)
updateOriginEndpointResponse_timeDelaySeconds = Lens.lens (\UpdateOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@UpdateOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_authorization :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Authorization)
updateOriginEndpointResponse_authorization = Lens.lens (\UpdateOriginEndpointResponse' {authorization} -> authorization) (\s@UpdateOriginEndpointResponse' {} a -> s {authorization = a} :: UpdateOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
updateOriginEndpointResponse_url :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
updateOriginEndpointResponse_url = Lens.lens (\UpdateOriginEndpointResponse' {url} -> url) (\s@UpdateOriginEndpointResponse' {} a -> s {url = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_hlsPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe HlsPackage)
updateOriginEndpointResponse_hlsPackage = Lens.lens (\UpdateOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {hlsPackage = a} :: UpdateOriginEndpointResponse)

-- | The response's http status code.
updateOriginEndpointResponse_httpStatus :: Lens.Lens' UpdateOriginEndpointResponse Core.Int
updateOriginEndpointResponse_httpStatus = Lens.lens (\UpdateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateOriginEndpointResponse' {} a -> s {httpStatus = a} :: UpdateOriginEndpointResponse)

instance Core.NFData UpdateOriginEndpointResponse
