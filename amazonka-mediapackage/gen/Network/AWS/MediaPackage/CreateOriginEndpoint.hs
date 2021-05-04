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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'newCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
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
    cmafPackage :: Prelude.Maybe CmafPackageCreateOrUpdateParameters,
    -- | A short string that will be used as the filename of the OriginEndpoint
    -- URL (defaults to \"index\").
    manifestName :: Prelude.Maybe Prelude.Text,
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
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The ID of the Channel that the OriginEndpoint will be associated with.
    -- This cannot be changed after the OriginEndpoint is created.
    channelId :: Prelude.Text,
    -- | The ID of the OriginEndpoint. The ID must be unique within the region
    -- and it cannot be changed after the OriginEndpoint is created.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  CreateOriginEndpoint
newCreateOriginEndpoint pChannelId_ pId_ =
  CreateOriginEndpoint'
    { dashPackage =
        Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      origination = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      whitelist = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      authorization = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      channelId = pChannelId_,
      id = pId_
    }

-- | Undocumented member.
createOriginEndpoint_dashPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe DashPackage)
createOriginEndpoint_dashPackage = Lens.lens (\CreateOriginEndpoint' {dashPackage} -> dashPackage) (\s@CreateOriginEndpoint' {} a -> s {dashPackage = a} :: CreateOriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpoint_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Int)
createOriginEndpoint_startoverWindowSeconds = Lens.lens (\CreateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpoint_origination :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Origination)
createOriginEndpoint_origination = Lens.lens (\CreateOriginEndpoint' {origination} -> origination) (\s@CreateOriginEndpoint' {} a -> s {origination = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_cmafPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe CmafPackageCreateOrUpdateParameters)
createOriginEndpoint_cmafPackage = Lens.lens (\CreateOriginEndpoint' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpoint' {} a -> s {cmafPackage = a} :: CreateOriginEndpoint)

-- | A short string that will be used as the filename of the OriginEndpoint
-- URL (defaults to \"index\").
createOriginEndpoint_manifestName :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_manifestName = Lens.lens (\CreateOriginEndpoint' {manifestName} -> manifestName) (\s@CreateOriginEndpoint' {} a -> s {manifestName = a} :: CreateOriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpoint_whitelist :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe [Prelude.Text])
createOriginEndpoint_whitelist = Lens.lens (\CreateOriginEndpoint' {whitelist} -> whitelist) (\s@CreateOriginEndpoint' {} a -> s {whitelist = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
createOriginEndpoint_mssPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe MssPackage)
createOriginEndpoint_mssPackage = Lens.lens (\CreateOriginEndpoint' {mssPackage} -> mssPackage) (\s@CreateOriginEndpoint' {} a -> s {mssPackage = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_tags :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpoint_tags = Lens.lens (\CreateOriginEndpoint' {tags} -> tags) (\s@CreateOriginEndpoint' {} a -> s {tags = a} :: CreateOriginEndpoint) Prelude.. Lens.mapping Prelude._Coerce

-- | A short text description of the OriginEndpoint.
createOriginEndpoint_description :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Text)
createOriginEndpoint_description = Lens.lens (\CreateOriginEndpoint' {description} -> description) (\s@CreateOriginEndpoint' {} a -> s {description = a} :: CreateOriginEndpoint)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpoint_timeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Prelude.Int)
createOriginEndpoint_timeDelaySeconds = Lens.lens (\CreateOriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpoint' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_authorization :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe Authorization)
createOriginEndpoint_authorization = Lens.lens (\CreateOriginEndpoint' {authorization} -> authorization) (\s@CreateOriginEndpoint' {} a -> s {authorization = a} :: CreateOriginEndpoint)

-- | Undocumented member.
createOriginEndpoint_hlsPackage :: Lens.Lens' CreateOriginEndpoint (Prelude.Maybe HlsPackage)
createOriginEndpoint_hlsPackage = Lens.lens (\CreateOriginEndpoint' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpoint' {} a -> s {hlsPackage = a} :: CreateOriginEndpoint)

-- | The ID of the Channel that the OriginEndpoint will be associated with.
-- This cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_channelId :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_channelId = Lens.lens (\CreateOriginEndpoint' {channelId} -> channelId) (\s@CreateOriginEndpoint' {} a -> s {channelId = a} :: CreateOriginEndpoint)

-- | The ID of the OriginEndpoint. The ID must be unique within the region
-- and it cannot be changed after the OriginEndpoint is created.
createOriginEndpoint_id :: Lens.Lens' CreateOriginEndpoint Prelude.Text
createOriginEndpoint_id = Lens.lens (\CreateOriginEndpoint' {id} -> id) (\s@CreateOriginEndpoint' {} a -> s {id = a} :: CreateOriginEndpoint)

instance Prelude.AWSRequest CreateOriginEndpoint where
  type
    Rs CreateOriginEndpoint =
      CreateOriginEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOriginEndpointResponse'
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

instance Prelude.Hashable CreateOriginEndpoint

instance Prelude.NFData CreateOriginEndpoint

instance Prelude.ToHeaders CreateOriginEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dashPackage" Prelude..=) Prelude.<$> dashPackage,
            ("startoverWindowSeconds" Prelude..=)
              Prelude.<$> startoverWindowSeconds,
            ("origination" Prelude..=) Prelude.<$> origination,
            ("cmafPackage" Prelude..=) Prelude.<$> cmafPackage,
            ("manifestName" Prelude..=) Prelude.<$> manifestName,
            ("whitelist" Prelude..=) Prelude.<$> whitelist,
            ("mssPackage" Prelude..=) Prelude.<$> mssPackage,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            ("timeDelaySeconds" Prelude..=)
              Prelude.<$> timeDelaySeconds,
            ("authorization" Prelude..=)
              Prelude.<$> authorization,
            ("hlsPackage" Prelude..=) Prelude.<$> hlsPackage,
            Prelude.Just ("channelId" Prelude..= channelId),
            Prelude.Just ("id" Prelude..= id)
          ]
      )

instance Prelude.ToPath CreateOriginEndpoint where
  toPath = Prelude.const "/origin_endpoints"

instance Prelude.ToQuery CreateOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
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
  Prelude.Int ->
  CreateOriginEndpointResponse
newCreateOriginEndpointResponse pHttpStatus_ =
  CreateOriginEndpointResponse'
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
createOriginEndpointResponse_dashPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe DashPackage)
createOriginEndpointResponse_dashPackage = Lens.lens (\CreateOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@CreateOriginEndpointResponse' {} a -> s {dashPackage = a} :: CreateOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
createOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
createOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\CreateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@CreateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: CreateOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
createOriginEndpointResponse_origination :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Origination)
createOriginEndpointResponse_origination = Lens.lens (\CreateOriginEndpointResponse' {origination} -> origination) (\s@CreateOriginEndpointResponse' {} a -> s {origination = a} :: CreateOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
createOriginEndpointResponse_channelId :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_channelId = Lens.lens (\CreateOriginEndpointResponse' {channelId} -> channelId) (\s@CreateOriginEndpointResponse' {} a -> s {channelId = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_cmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe CmafPackage)
createOriginEndpointResponse_cmafPackage = Lens.lens (\CreateOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@CreateOriginEndpointResponse' {} a -> s {cmafPackage = a} :: CreateOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
createOriginEndpointResponse_manifestName :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_manifestName = Lens.lens (\CreateOriginEndpointResponse' {manifestName} -> manifestName) (\s@CreateOriginEndpointResponse' {} a -> s {manifestName = a} :: CreateOriginEndpointResponse)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
createOriginEndpointResponse_arn :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_arn = Lens.lens (\CreateOriginEndpointResponse' {arn} -> arn) (\s@CreateOriginEndpointResponse' {} a -> s {arn = a} :: CreateOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
createOriginEndpointResponse_id :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_id = Lens.lens (\CreateOriginEndpointResponse' {id} -> id) (\s@CreateOriginEndpointResponse' {} a -> s {id = a} :: CreateOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
createOriginEndpointResponse_whitelist :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
createOriginEndpointResponse_whitelist = Lens.lens (\CreateOriginEndpointResponse' {whitelist} -> whitelist) (\s@CreateOriginEndpointResponse' {} a -> s {whitelist = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
createOriginEndpointResponse_mssPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe MssPackage)
createOriginEndpointResponse_mssPackage = Lens.lens (\CreateOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@CreateOriginEndpointResponse' {} a -> s {mssPackage = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_tags :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOriginEndpointResponse_tags = Lens.lens (\CreateOriginEndpointResponse' {tags} -> tags) (\s@CreateOriginEndpointResponse' {} a -> s {tags = a} :: CreateOriginEndpointResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A short text description of the OriginEndpoint.
createOriginEndpointResponse_description :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_description = Lens.lens (\CreateOriginEndpointResponse' {description} -> description) (\s@CreateOriginEndpointResponse' {} a -> s {description = a} :: CreateOriginEndpointResponse)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
createOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
createOriginEndpointResponse_timeDelaySeconds = Lens.lens (\CreateOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@CreateOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_authorization :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Authorization)
createOriginEndpointResponse_authorization = Lens.lens (\CreateOriginEndpointResponse' {authorization} -> authorization) (\s@CreateOriginEndpointResponse' {} a -> s {authorization = a} :: CreateOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
createOriginEndpointResponse_url :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
createOriginEndpointResponse_url = Lens.lens (\CreateOriginEndpointResponse' {url} -> url) (\s@CreateOriginEndpointResponse' {} a -> s {url = a} :: CreateOriginEndpointResponse)

-- | Undocumented member.
createOriginEndpointResponse_hlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Prelude.Maybe HlsPackage)
createOriginEndpointResponse_hlsPackage = Lens.lens (\CreateOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@CreateOriginEndpointResponse' {} a -> s {hlsPackage = a} :: CreateOriginEndpointResponse)

-- | The response's http status code.
createOriginEndpointResponse_httpStatus :: Lens.Lens' CreateOriginEndpointResponse Prelude.Int
createOriginEndpointResponse_httpStatus = Lens.lens (\CreateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateOriginEndpointResponse' {} a -> s {httpStatus = a} :: CreateOriginEndpointResponse)

instance Prelude.NFData CreateOriginEndpointResponse
