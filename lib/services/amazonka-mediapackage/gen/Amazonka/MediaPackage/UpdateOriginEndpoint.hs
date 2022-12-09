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
-- Module      : Amazonka.MediaPackage.UpdateOriginEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing OriginEndpoint.
module Amazonka.MediaPackage.UpdateOriginEndpoint
  ( -- * Creating a Request
    UpdateOriginEndpoint (..),
    newUpdateOriginEndpoint,

    -- * Request Lenses
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_description,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_id,

    -- * Destructuring the Response
    UpdateOriginEndpointResponse (..),
    newUpdateOriginEndpointResponse,

    -- * Response Lenses
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Configuration parameters used to update an existing OriginEndpoint.
--
-- /See:/ 'newUpdateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { authorization :: Prelude.Maybe Authorization,
    cmafPackage :: Prelude.Maybe CmafPackageCreateOrUpdateParameters,
    dashPackage :: Prelude.Maybe DashPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Prelude.Maybe Prelude.Text,
    hlsPackage :: Prelude.Maybe HlsPackage,
    -- | A short string that will be appended to the end of the Endpoint URL.
    manifestName :: Prelude.Maybe Prelude.Text,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint.
    -- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
    -- other form of access control. If set to DENY, the OriginEndpoint may not
    -- be requested. This can be helpful for Live to VOD harvesting, or for
    -- temporarily disabling origination
    origination :: Prelude.Maybe Origination,
    -- | Maximum duration (in seconds) of content to retain for startover
    -- playback. If not specified, startover playback will be disabled for the
    -- OriginEndpoint.
    startoverWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | Amount of delay (in seconds) to enforce on the playback of live content.
    -- If not specified, there will be no time delay in effect for the
    -- OriginEndpoint.
    timeDelaySeconds :: Prelude.Maybe Prelude.Int,
    -- | A list of source IP CIDR blocks that will be allowed to access the
    -- OriginEndpoint.
    whitelist :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the OriginEndpoint to update.
    id :: Prelude.Text
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
-- 'authorization', 'updateOriginEndpoint_authorization' - Undocumented member.
--
-- 'cmafPackage', 'updateOriginEndpoint_cmafPackage' - Undocumented member.
--
-- 'dashPackage', 'updateOriginEndpoint_dashPackage' - Undocumented member.
--
-- 'description', 'updateOriginEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'hlsPackage', 'updateOriginEndpoint_hlsPackage' - Undocumented member.
--
-- 'manifestName', 'updateOriginEndpoint_manifestName' - A short string that will be appended to the end of the Endpoint URL.
--
-- 'mssPackage', 'updateOriginEndpoint_mssPackage' - Undocumented member.
--
-- 'origination', 'updateOriginEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'startoverWindowSeconds', 'updateOriginEndpoint_startoverWindowSeconds' - Maximum duration (in seconds) of content to retain for startover
-- playback. If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'timeDelaySeconds', 'updateOriginEndpoint_timeDelaySeconds' - Amount of delay (in seconds) to enforce on the playback of live content.
-- If not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'whitelist', 'updateOriginEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'id', 'updateOriginEndpoint_id' - The ID of the OriginEndpoint to update.
newUpdateOriginEndpoint ::
  -- | 'id'
  Prelude.Text ->
  UpdateOriginEndpoint
newUpdateOriginEndpoint pId_ =
  UpdateOriginEndpoint'
    { authorization =
        Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      description = Prelude.Nothing,
      hlsPackage = Prelude.Nothing,
      manifestName = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      origination = Prelude.Nothing,
      startoverWindowSeconds = Prelude.Nothing,
      timeDelaySeconds = Prelude.Nothing,
      whitelist = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateOriginEndpoint_authorization :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Authorization)
updateOriginEndpoint_authorization = Lens.lens (\UpdateOriginEndpoint' {authorization} -> authorization) (\s@UpdateOriginEndpoint' {} a -> s {authorization = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_cmafPackage :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe CmafPackageCreateOrUpdateParameters)
updateOriginEndpoint_cmafPackage = Lens.lens (\UpdateOriginEndpoint' {cmafPackage} -> cmafPackage) (\s@UpdateOriginEndpoint' {} a -> s {cmafPackage = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_dashPackage :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe DashPackage)
updateOriginEndpoint_dashPackage = Lens.lens (\UpdateOriginEndpoint' {dashPackage} -> dashPackage) (\s@UpdateOriginEndpoint' {} a -> s {dashPackage = a} :: UpdateOriginEndpoint)

-- | A short text description of the OriginEndpoint.
updateOriginEndpoint_description :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Text)
updateOriginEndpoint_description = Lens.lens (\UpdateOriginEndpoint' {description} -> description) (\s@UpdateOriginEndpoint' {} a -> s {description = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_hlsPackage :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe HlsPackage)
updateOriginEndpoint_hlsPackage = Lens.lens (\UpdateOriginEndpoint' {hlsPackage} -> hlsPackage) (\s@UpdateOriginEndpoint' {} a -> s {hlsPackage = a} :: UpdateOriginEndpoint)

-- | A short string that will be appended to the end of the Endpoint URL.
updateOriginEndpoint_manifestName :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Text)
updateOriginEndpoint_manifestName = Lens.lens (\UpdateOriginEndpoint' {manifestName} -> manifestName) (\s@UpdateOriginEndpoint' {} a -> s {manifestName = a} :: UpdateOriginEndpoint)

-- | Undocumented member.
updateOriginEndpoint_mssPackage :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe MssPackage)
updateOriginEndpoint_mssPackage = Lens.lens (\UpdateOriginEndpoint' {mssPackage} -> mssPackage) (\s@UpdateOriginEndpoint' {} a -> s {mssPackage = a} :: UpdateOriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
updateOriginEndpoint_origination :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Origination)
updateOriginEndpoint_origination = Lens.lens (\UpdateOriginEndpoint' {origination} -> origination) (\s@UpdateOriginEndpoint' {} a -> s {origination = a} :: UpdateOriginEndpoint)

-- | Maximum duration (in seconds) of content to retain for startover
-- playback. If not specified, startover playback will be disabled for the
-- OriginEndpoint.
updateOriginEndpoint_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Int)
updateOriginEndpoint_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpoint)

-- | Amount of delay (in seconds) to enforce on the playback of live content.
-- If not specified, there will be no time delay in effect for the
-- OriginEndpoint.
updateOriginEndpoint_timeDelaySeconds :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe Prelude.Int)
updateOriginEndpoint_timeDelaySeconds = Lens.lens (\UpdateOriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@UpdateOriginEndpoint' {} a -> s {timeDelaySeconds = a} :: UpdateOriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
updateOriginEndpoint_whitelist :: Lens.Lens' UpdateOriginEndpoint (Prelude.Maybe [Prelude.Text])
updateOriginEndpoint_whitelist = Lens.lens (\UpdateOriginEndpoint' {whitelist} -> whitelist) (\s@UpdateOriginEndpoint' {} a -> s {whitelist = a} :: UpdateOriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the OriginEndpoint to update.
updateOriginEndpoint_id :: Lens.Lens' UpdateOriginEndpoint Prelude.Text
updateOriginEndpoint_id = Lens.lens (\UpdateOriginEndpoint' {id} -> id) (\s@UpdateOriginEndpoint' {} a -> s {id = a} :: UpdateOriginEndpoint)

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

instance Prelude.Hashable UpdateOriginEndpoint where
  hashWithSalt _salt UpdateOriginEndpoint' {..} =
    _salt `Prelude.hashWithSalt` authorization
      `Prelude.hashWithSalt` cmafPackage
      `Prelude.hashWithSalt` dashPackage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hlsPackage
      `Prelude.hashWithSalt` manifestName
      `Prelude.hashWithSalt` mssPackage
      `Prelude.hashWithSalt` origination
      `Prelude.hashWithSalt` startoverWindowSeconds
      `Prelude.hashWithSalt` timeDelaySeconds
      `Prelude.hashWithSalt` whitelist
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateOriginEndpoint where
  rnf UpdateOriginEndpoint' {..} =
    Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf cmafPackage
      `Prelude.seq` Prelude.rnf dashPackage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsPackage
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf mssPackage
      `Prelude.seq` Prelude.rnf origination
      `Prelude.seq` Prelude.rnf startoverWindowSeconds
      `Prelude.seq` Prelude.rnf timeDelaySeconds
      `Prelude.seq` Prelude.rnf whitelist
      `Prelude.seq` Prelude.rnf id

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
          [ ("authorization" Data..=) Prelude.<$> authorization,
            ("cmafPackage" Data..=) Prelude.<$> cmafPackage,
            ("dashPackage" Data..=) Prelude.<$> dashPackage,
            ("description" Data..=) Prelude.<$> description,
            ("hlsPackage" Data..=) Prelude.<$> hlsPackage,
            ("manifestName" Data..=) Prelude.<$> manifestName,
            ("mssPackage" Data..=) Prelude.<$> mssPackage,
            ("origination" Data..=) Prelude.<$> origination,
            ("startoverWindowSeconds" Data..=)
              Prelude.<$> startoverWindowSeconds,
            ("timeDelaySeconds" Data..=)
              Prelude.<$> timeDelaySeconds,
            ("whitelist" Data..=) Prelude.<$> whitelist
          ]
      )

instance Data.ToPath UpdateOriginEndpoint where
  toPath UpdateOriginEndpoint' {..} =
    Prelude.mconcat
      ["/origin_endpoints/", Data.toBS id]

instance Data.ToQuery UpdateOriginEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
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
-- Create a value of 'UpdateOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateOriginEndpointResponse_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'authorization', 'updateOriginEndpointResponse_authorization' - Undocumented member.
--
-- 'channelId', 'updateOriginEndpointResponse_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'updateOriginEndpointResponse_cmafPackage' - Undocumented member.
--
-- 'dashPackage', 'updateOriginEndpointResponse_dashPackage' - Undocumented member.
--
-- 'description', 'updateOriginEndpointResponse_description' - A short text description of the OriginEndpoint.
--
-- 'hlsPackage', 'updateOriginEndpointResponse_hlsPackage' - Undocumented member.
--
-- 'id', 'updateOriginEndpointResponse_id' - The ID of the OriginEndpoint.
--
-- 'manifestName', 'updateOriginEndpointResponse_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'mssPackage', 'updateOriginEndpointResponse_mssPackage' - Undocumented member.
--
-- 'origination', 'updateOriginEndpointResponse_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'startoverWindowSeconds', 'updateOriginEndpointResponse_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'tags', 'updateOriginEndpointResponse_tags' - Undocumented member.
--
-- 'timeDelaySeconds', 'updateOriginEndpointResponse_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'url', 'updateOriginEndpointResponse_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'whitelist', 'updateOriginEndpointResponse_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'httpStatus', 'updateOriginEndpointResponse_httpStatus' - The response's http status code.
newUpdateOriginEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOriginEndpointResponse
newUpdateOriginEndpointResponse pHttpStatus_ =
  UpdateOriginEndpointResponse'
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
updateOriginEndpointResponse_arn :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_arn = Lens.lens (\UpdateOriginEndpointResponse' {arn} -> arn) (\s@UpdateOriginEndpointResponse' {} a -> s {arn = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_authorization :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Authorization)
updateOriginEndpointResponse_authorization = Lens.lens (\UpdateOriginEndpointResponse' {authorization} -> authorization) (\s@UpdateOriginEndpointResponse' {} a -> s {authorization = a} :: UpdateOriginEndpointResponse)

-- | The ID of the Channel the OriginEndpoint is associated with.
updateOriginEndpointResponse_channelId :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_channelId = Lens.lens (\UpdateOriginEndpointResponse' {channelId} -> channelId) (\s@UpdateOriginEndpointResponse' {} a -> s {channelId = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_cmafPackage :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe CmafPackage)
updateOriginEndpointResponse_cmafPackage = Lens.lens (\UpdateOriginEndpointResponse' {cmafPackage} -> cmafPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {cmafPackage = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_dashPackage :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe DashPackage)
updateOriginEndpointResponse_dashPackage = Lens.lens (\UpdateOriginEndpointResponse' {dashPackage} -> dashPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {dashPackage = a} :: UpdateOriginEndpointResponse)

-- | A short text description of the OriginEndpoint.
updateOriginEndpointResponse_description :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_description = Lens.lens (\UpdateOriginEndpointResponse' {description} -> description) (\s@UpdateOriginEndpointResponse' {} a -> s {description = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_hlsPackage :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe HlsPackage)
updateOriginEndpointResponse_hlsPackage = Lens.lens (\UpdateOriginEndpointResponse' {hlsPackage} -> hlsPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {hlsPackage = a} :: UpdateOriginEndpointResponse)

-- | The ID of the OriginEndpoint.
updateOriginEndpointResponse_id :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_id = Lens.lens (\UpdateOriginEndpointResponse' {id} -> id) (\s@UpdateOriginEndpointResponse' {} a -> s {id = a} :: UpdateOriginEndpointResponse)

-- | A short string appended to the end of the OriginEndpoint URL.
updateOriginEndpointResponse_manifestName :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_manifestName = Lens.lens (\UpdateOriginEndpointResponse' {manifestName} -> manifestName) (\s@UpdateOriginEndpointResponse' {} a -> s {manifestName = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_mssPackage :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe MssPackage)
updateOriginEndpointResponse_mssPackage = Lens.lens (\UpdateOriginEndpointResponse' {mssPackage} -> mssPackage) (\s@UpdateOriginEndpointResponse' {} a -> s {mssPackage = a} :: UpdateOriginEndpointResponse)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
updateOriginEndpointResponse_origination :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Origination)
updateOriginEndpointResponse_origination = Lens.lens (\UpdateOriginEndpointResponse' {origination} -> origination) (\s@UpdateOriginEndpointResponse' {} a -> s {origination = a} :: UpdateOriginEndpointResponse)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
updateOriginEndpointResponse_startoverWindowSeconds :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
updateOriginEndpointResponse_startoverWindowSeconds = Lens.lens (\UpdateOriginEndpointResponse' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@UpdateOriginEndpointResponse' {} a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpointResponse)

-- | Undocumented member.
updateOriginEndpointResponse_tags :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateOriginEndpointResponse_tags = Lens.lens (\UpdateOriginEndpointResponse' {tags} -> tags) (\s@UpdateOriginEndpointResponse' {} a -> s {tags = a} :: UpdateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
updateOriginEndpointResponse_timeDelaySeconds :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Int)
updateOriginEndpointResponse_timeDelaySeconds = Lens.lens (\UpdateOriginEndpointResponse' {timeDelaySeconds} -> timeDelaySeconds) (\s@UpdateOriginEndpointResponse' {} a -> s {timeDelaySeconds = a} :: UpdateOriginEndpointResponse)

-- | The URL of the packaged OriginEndpoint for consumption.
updateOriginEndpointResponse_url :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe Prelude.Text)
updateOriginEndpointResponse_url = Lens.lens (\UpdateOriginEndpointResponse' {url} -> url) (\s@UpdateOriginEndpointResponse' {} a -> s {url = a} :: UpdateOriginEndpointResponse)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
updateOriginEndpointResponse_whitelist :: Lens.Lens' UpdateOriginEndpointResponse (Prelude.Maybe [Prelude.Text])
updateOriginEndpointResponse_whitelist = Lens.lens (\UpdateOriginEndpointResponse' {whitelist} -> whitelist) (\s@UpdateOriginEndpointResponse' {} a -> s {whitelist = a} :: UpdateOriginEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateOriginEndpointResponse_httpStatus :: Lens.Lens' UpdateOriginEndpointResponse Prelude.Int
updateOriginEndpointResponse_httpStatus = Lens.lens (\UpdateOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateOriginEndpointResponse' {} a -> s {httpStatus = a} :: UpdateOriginEndpointResponse)

instance Prelude.NFData UpdateOriginEndpointResponse where
  rnf UpdateOriginEndpointResponse' {..} =
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
