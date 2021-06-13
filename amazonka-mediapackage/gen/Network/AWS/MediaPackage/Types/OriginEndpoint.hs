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
import qualified Network.AWS.Prelude as Prelude

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'newOriginEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
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
    hlsPackage :: Prelude.Maybe HlsPackage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { dashPackage = Prelude.Nothing,
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
      hlsPackage = Prelude.Nothing
    }

-- | Undocumented member.
originEndpoint_dashPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe DashPackage)
originEndpoint_dashPackage = Lens.lens (\OriginEndpoint' {dashPackage} -> dashPackage) (\s@OriginEndpoint' {} a -> s {dashPackage = a} :: OriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
originEndpoint_startoverWindowSeconds :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Int)
originEndpoint_startoverWindowSeconds = Lens.lens (\OriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@OriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: OriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
originEndpoint_origination :: Lens.Lens' OriginEndpoint (Prelude.Maybe Origination)
originEndpoint_origination = Lens.lens (\OriginEndpoint' {origination} -> origination) (\s@OriginEndpoint' {} a -> s {origination = a} :: OriginEndpoint)

-- | The ID of the Channel the OriginEndpoint is associated with.
originEndpoint_channelId :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_channelId = Lens.lens (\OriginEndpoint' {channelId} -> channelId) (\s@OriginEndpoint' {} a -> s {channelId = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_cmafPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe CmafPackage)
originEndpoint_cmafPackage = Lens.lens (\OriginEndpoint' {cmafPackage} -> cmafPackage) (\s@OriginEndpoint' {} a -> s {cmafPackage = a} :: OriginEndpoint)

-- | A short string appended to the end of the OriginEndpoint URL.
originEndpoint_manifestName :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_manifestName = Lens.lens (\OriginEndpoint' {manifestName} -> manifestName) (\s@OriginEndpoint' {} a -> s {manifestName = a} :: OriginEndpoint)

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
originEndpoint_arn :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_arn = Lens.lens (\OriginEndpoint' {arn} -> arn) (\s@OriginEndpoint' {} a -> s {arn = a} :: OriginEndpoint)

-- | The ID of the OriginEndpoint.
originEndpoint_id :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_id = Lens.lens (\OriginEndpoint' {id} -> id) (\s@OriginEndpoint' {} a -> s {id = a} :: OriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
originEndpoint_whitelist :: Lens.Lens' OriginEndpoint (Prelude.Maybe [Prelude.Text])
originEndpoint_whitelist = Lens.lens (\OriginEndpoint' {whitelist} -> whitelist) (\s@OriginEndpoint' {} a -> s {whitelist = a} :: OriginEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
originEndpoint_mssPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe MssPackage)
originEndpoint_mssPackage = Lens.lens (\OriginEndpoint' {mssPackage} -> mssPackage) (\s@OriginEndpoint' {} a -> s {mssPackage = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_tags :: Lens.Lens' OriginEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
originEndpoint_tags = Lens.lens (\OriginEndpoint' {tags} -> tags) (\s@OriginEndpoint' {} a -> s {tags = a} :: OriginEndpoint) Prelude.. Lens.mapping Lens._Coerce

-- | A short text description of the OriginEndpoint.
originEndpoint_description :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_description = Lens.lens (\OriginEndpoint' {description} -> description) (\s@OriginEndpoint' {} a -> s {description = a} :: OriginEndpoint)

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
originEndpoint_timeDelaySeconds :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Int)
originEndpoint_timeDelaySeconds = Lens.lens (\OriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@OriginEndpoint' {} a -> s {timeDelaySeconds = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_authorization :: Lens.Lens' OriginEndpoint (Prelude.Maybe Authorization)
originEndpoint_authorization = Lens.lens (\OriginEndpoint' {authorization} -> authorization) (\s@OriginEndpoint' {} a -> s {authorization = a} :: OriginEndpoint)

-- | The URL of the packaged OriginEndpoint for consumption.
originEndpoint_url :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_url = Lens.lens (\OriginEndpoint' {url} -> url) (\s@OriginEndpoint' {} a -> s {url = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_hlsPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe HlsPackage)
originEndpoint_hlsPackage = Lens.lens (\OriginEndpoint' {hlsPackage} -> hlsPackage) (\s@OriginEndpoint' {} a -> s {hlsPackage = a} :: OriginEndpoint)

instance Core.FromJSON OriginEndpoint where
  parseJSON =
    Core.withObject
      "OriginEndpoint"
      ( \x ->
          OriginEndpoint'
            Prelude.<$> (x Core..:? "dashPackage")
            Prelude.<*> (x Core..:? "startoverWindowSeconds")
            Prelude.<*> (x Core..:? "origination")
            Prelude.<*> (x Core..:? "channelId")
            Prelude.<*> (x Core..:? "cmafPackage")
            Prelude.<*> (x Core..:? "manifestName")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "whitelist" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "mssPackage")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "timeDelaySeconds")
            Prelude.<*> (x Core..:? "authorization")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "hlsPackage")
      )

instance Prelude.Hashable OriginEndpoint

instance Prelude.NFData OriginEndpoint
