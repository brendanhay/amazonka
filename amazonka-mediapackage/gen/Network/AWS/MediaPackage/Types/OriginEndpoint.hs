{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.OriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.OriginEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.Authorization
import Network.AWS.MediaPackage.Types.CmafPackage
import Network.AWS.MediaPackage.Types.DashPackage
import Network.AWS.MediaPackage.Types.HlsPackage
import Network.AWS.MediaPackage.Types.MssPackage
import Network.AWS.MediaPackage.Types.Origination

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'newOriginEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
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
    hlsPackage :: Core.Maybe HlsPackage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashPackage', 'originEndpoint_dashPackage' - Undocumented member.
--
-- 'startoverWindowSeconds', 'originEndpoint_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'origination', 'originEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'channelId', 'originEndpoint_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'originEndpoint_cmafPackage' - Undocumented member.
--
-- 'manifestName', 'originEndpoint_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'arn', 'originEndpoint_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'id', 'originEndpoint_id' - The ID of the OriginEndpoint.
--
-- 'whitelist', 'originEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
--
-- 'mssPackage', 'originEndpoint_mssPackage' - Undocumented member.
--
-- 'tags', 'originEndpoint_tags' - Undocumented member.
--
-- 'description', 'originEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'timeDelaySeconds', 'originEndpoint_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'authorization', 'originEndpoint_authorization' - Undocumented member.
--
-- 'url', 'originEndpoint_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'hlsPackage', 'originEndpoint_hlsPackage' - Undocumented member.
newOriginEndpoint ::
  OriginEndpoint
newOriginEndpoint =
  OriginEndpoint'
    { dashPackage = Core.Nothing,
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
      hlsPackage = Core.Nothing
    }

-- | Undocumented member.
originEndpoint_dashPackage :: Lens.Lens' OriginEndpoint (Core.Maybe DashPackage)
originEndpoint_dashPackage = Lens.lens (\OriginEndpoint' {dashPackage} -> dashPackage) (\s@OriginEndpoint' {} a -> s {dashPackage = a} :: OriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
originEndpoint_startoverWindowSeconds :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Int)
originEndpoint_startoverWindowSeconds = Lens.lens (\OriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@OriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: OriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
originEndpoint_origination :: Lens.Lens' OriginEndpoint (Core.Maybe Origination)
originEndpoint_origination = Lens.lens (\OriginEndpoint' {origination} -> origination) (\s@OriginEndpoint' {} a -> s {origination = a} :: OriginEndpoint)

-- | The ID of the Channel the OriginEndpoint is associated with.
originEndpoint_channelId :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_channelId = Lens.lens (\OriginEndpoint' {channelId} -> channelId) (\s@OriginEndpoint' {} a -> s {channelId = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_cmafPackage :: Lens.Lens' OriginEndpoint (Core.Maybe CmafPackage)
originEndpoint_cmafPackage = Lens.lens (\OriginEndpoint' {cmafPackage} -> cmafPackage) (\s@OriginEndpoint' {} a -> s {cmafPackage = a} :: OriginEndpoint)

-- | A short string appended to the end of the OriginEndpoint URL.
originEndpoint_manifestName :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_manifestName = Lens.lens (\OriginEndpoint' {manifestName} -> manifestName) (\s@OriginEndpoint' {} a -> s {manifestName = a} :: OriginEndpoint)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
originEndpoint_arn :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_arn = Lens.lens (\OriginEndpoint' {arn} -> arn) (\s@OriginEndpoint' {} a -> s {arn = a} :: OriginEndpoint)

-- | The ID of the OriginEndpoint.
originEndpoint_id :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_id = Lens.lens (\OriginEndpoint' {id} -> id) (\s@OriginEndpoint' {} a -> s {id = a} :: OriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
originEndpoint_whitelist :: Lens.Lens' OriginEndpoint (Core.Maybe [Core.Text])
originEndpoint_whitelist = Lens.lens (\OriginEndpoint' {whitelist} -> whitelist) (\s@OriginEndpoint' {} a -> s {whitelist = a} :: OriginEndpoint) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
originEndpoint_mssPackage :: Lens.Lens' OriginEndpoint (Core.Maybe MssPackage)
originEndpoint_mssPackage = Lens.lens (\OriginEndpoint' {mssPackage} -> mssPackage) (\s@OriginEndpoint' {} a -> s {mssPackage = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_tags :: Lens.Lens' OriginEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
originEndpoint_tags = Lens.lens (\OriginEndpoint' {tags} -> tags) (\s@OriginEndpoint' {} a -> s {tags = a} :: OriginEndpoint) Core.. Lens.mapping Lens._Coerce

-- | A short text description of the OriginEndpoint.
originEndpoint_description :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_description = Lens.lens (\OriginEndpoint' {description} -> description) (\s@OriginEndpoint' {} a -> s {description = a} :: OriginEndpoint)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
originEndpoint_timeDelaySeconds :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Int)
originEndpoint_timeDelaySeconds = Lens.lens (\OriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@OriginEndpoint' {} a -> s {timeDelaySeconds = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_authorization :: Lens.Lens' OriginEndpoint (Core.Maybe Authorization)
originEndpoint_authorization = Lens.lens (\OriginEndpoint' {authorization} -> authorization) (\s@OriginEndpoint' {} a -> s {authorization = a} :: OriginEndpoint)

-- | The URL of the packaged OriginEndpoint for consumption.
originEndpoint_url :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
originEndpoint_url = Lens.lens (\OriginEndpoint' {url} -> url) (\s@OriginEndpoint' {} a -> s {url = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_hlsPackage :: Lens.Lens' OriginEndpoint (Core.Maybe HlsPackage)
originEndpoint_hlsPackage = Lens.lens (\OriginEndpoint' {hlsPackage} -> hlsPackage) (\s@OriginEndpoint' {} a -> s {hlsPackage = a} :: OriginEndpoint)

instance Core.FromJSON OriginEndpoint where
  parseJSON =
    Core.withObject
      "OriginEndpoint"
      ( \x ->
          OriginEndpoint'
            Core.<$> (x Core..:? "dashPackage")
            Core.<*> (x Core..:? "startoverWindowSeconds")
            Core.<*> (x Core..:? "origination")
            Core.<*> (x Core..:? "channelId")
            Core.<*> (x Core..:? "cmafPackage")
            Core.<*> (x Core..:? "manifestName")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "whitelist" Core..!= Core.mempty)
            Core.<*> (x Core..:? "mssPackage")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "timeDelaySeconds")
            Core.<*> (x Core..:? "authorization")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "hlsPackage")
      )

instance Core.Hashable OriginEndpoint

instance Core.NFData OriginEndpoint
