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
-- Module      : Amazonka.MediaPackage.Types.OriginEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.OriginEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.Authorization
import Amazonka.MediaPackage.Types.CmafPackage
import Amazonka.MediaPackage.Types.DashPackage
import Amazonka.MediaPackage.Types.HlsPackage
import Amazonka.MediaPackage.Types.MssPackage
import Amazonka.MediaPackage.Types.Origination
import qualified Amazonka.Prelude as Prelude

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'newOriginEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
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
    whitelist :: Prelude.Maybe [Prelude.Text]
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
-- 'arn', 'originEndpoint_arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- 'authorization', 'originEndpoint_authorization' - Undocumented member.
--
-- 'channelId', 'originEndpoint_channelId' - The ID of the Channel the OriginEndpoint is associated with.
--
-- 'cmafPackage', 'originEndpoint_cmafPackage' - Undocumented member.
--
-- 'dashPackage', 'originEndpoint_dashPackage' - Undocumented member.
--
-- 'description', 'originEndpoint_description' - A short text description of the OriginEndpoint.
--
-- 'hlsPackage', 'originEndpoint_hlsPackage' - Undocumented member.
--
-- 'id', 'originEndpoint_id' - The ID of the OriginEndpoint.
--
-- 'manifestName', 'originEndpoint_manifestName' - A short string appended to the end of the OriginEndpoint URL.
--
-- 'mssPackage', 'originEndpoint_mssPackage' - Undocumented member.
--
-- 'origination', 'originEndpoint_origination' - Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
--
-- 'startoverWindowSeconds', 'originEndpoint_startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
--
-- 'tags', 'originEndpoint_tags' - Undocumented member.
--
-- 'timeDelaySeconds', 'originEndpoint_timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
--
-- 'url', 'originEndpoint_url' - The URL of the packaged OriginEndpoint for consumption.
--
-- 'whitelist', 'originEndpoint_whitelist' - A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
newOriginEndpoint ::
  OriginEndpoint
newOriginEndpoint =
  OriginEndpoint'
    { arn = Prelude.Nothing,
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
      whitelist = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
originEndpoint_arn :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_arn = Lens.lens (\OriginEndpoint' {arn} -> arn) (\s@OriginEndpoint' {} a -> s {arn = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_authorization :: Lens.Lens' OriginEndpoint (Prelude.Maybe Authorization)
originEndpoint_authorization = Lens.lens (\OriginEndpoint' {authorization} -> authorization) (\s@OriginEndpoint' {} a -> s {authorization = a} :: OriginEndpoint)

-- | The ID of the Channel the OriginEndpoint is associated with.
originEndpoint_channelId :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_channelId = Lens.lens (\OriginEndpoint' {channelId} -> channelId) (\s@OriginEndpoint' {} a -> s {channelId = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_cmafPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe CmafPackage)
originEndpoint_cmafPackage = Lens.lens (\OriginEndpoint' {cmafPackage} -> cmafPackage) (\s@OriginEndpoint' {} a -> s {cmafPackage = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_dashPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe DashPackage)
originEndpoint_dashPackage = Lens.lens (\OriginEndpoint' {dashPackage} -> dashPackage) (\s@OriginEndpoint' {} a -> s {dashPackage = a} :: OriginEndpoint)

-- | A short text description of the OriginEndpoint.
originEndpoint_description :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_description = Lens.lens (\OriginEndpoint' {description} -> description) (\s@OriginEndpoint' {} a -> s {description = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_hlsPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe HlsPackage)
originEndpoint_hlsPackage = Lens.lens (\OriginEndpoint' {hlsPackage} -> hlsPackage) (\s@OriginEndpoint' {} a -> s {hlsPackage = a} :: OriginEndpoint)

-- | The ID of the OriginEndpoint.
originEndpoint_id :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_id = Lens.lens (\OriginEndpoint' {id} -> id) (\s@OriginEndpoint' {} a -> s {id = a} :: OriginEndpoint)

-- | A short string appended to the end of the OriginEndpoint URL.
originEndpoint_manifestName :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_manifestName = Lens.lens (\OriginEndpoint' {manifestName} -> manifestName) (\s@OriginEndpoint' {} a -> s {manifestName = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_mssPackage :: Lens.Lens' OriginEndpoint (Prelude.Maybe MssPackage)
originEndpoint_mssPackage = Lens.lens (\OriginEndpoint' {mssPackage} -> mssPackage) (\s@OriginEndpoint' {} a -> s {mssPackage = a} :: OriginEndpoint)

-- | Control whether origination of video is allowed for this OriginEndpoint.
-- If set to ALLOW, the OriginEndpoint may by requested, pursuant to any
-- other form of access control. If set to DENY, the OriginEndpoint may not
-- be requested. This can be helpful for Live to VOD harvesting, or for
-- temporarily disabling origination
originEndpoint_origination :: Lens.Lens' OriginEndpoint (Prelude.Maybe Origination)
originEndpoint_origination = Lens.lens (\OriginEndpoint' {origination} -> origination) (\s@OriginEndpoint' {} a -> s {origination = a} :: OriginEndpoint)

-- | Maximum duration (seconds) of content to retain for startover playback.
-- If not specified, startover playback will be disabled for the
-- OriginEndpoint.
originEndpoint_startoverWindowSeconds :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Int)
originEndpoint_startoverWindowSeconds = Lens.lens (\OriginEndpoint' {startoverWindowSeconds} -> startoverWindowSeconds) (\s@OriginEndpoint' {} a -> s {startoverWindowSeconds = a} :: OriginEndpoint)

-- | Undocumented member.
originEndpoint_tags :: Lens.Lens' OriginEndpoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
originEndpoint_tags = Lens.lens (\OriginEndpoint' {tags} -> tags) (\s@OriginEndpoint' {} a -> s {tags = a} :: OriginEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Amount of delay (seconds) to enforce on the playback of live content. If
-- not specified, there will be no time delay in effect for the
-- OriginEndpoint.
originEndpoint_timeDelaySeconds :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Int)
originEndpoint_timeDelaySeconds = Lens.lens (\OriginEndpoint' {timeDelaySeconds} -> timeDelaySeconds) (\s@OriginEndpoint' {} a -> s {timeDelaySeconds = a} :: OriginEndpoint)

-- | The URL of the packaged OriginEndpoint for consumption.
originEndpoint_url :: Lens.Lens' OriginEndpoint (Prelude.Maybe Prelude.Text)
originEndpoint_url = Lens.lens (\OriginEndpoint' {url} -> url) (\s@OriginEndpoint' {} a -> s {url = a} :: OriginEndpoint)

-- | A list of source IP CIDR blocks that will be allowed to access the
-- OriginEndpoint.
originEndpoint_whitelist :: Lens.Lens' OriginEndpoint (Prelude.Maybe [Prelude.Text])
originEndpoint_whitelist = Lens.lens (\OriginEndpoint' {whitelist} -> whitelist) (\s@OriginEndpoint' {} a -> s {whitelist = a} :: OriginEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OriginEndpoint where
  parseJSON =
    Data.withObject
      "OriginEndpoint"
      ( \x ->
          OriginEndpoint'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authorization")
            Prelude.<*> (x Data..:? "channelId")
            Prelude.<*> (x Data..:? "cmafPackage")
            Prelude.<*> (x Data..:? "dashPackage")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "hlsPackage")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "manifestName")
            Prelude.<*> (x Data..:? "mssPackage")
            Prelude.<*> (x Data..:? "origination")
            Prelude.<*> (x Data..:? "startoverWindowSeconds")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "timeDelaySeconds")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "whitelist" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OriginEndpoint where
  hashWithSalt _salt OriginEndpoint' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authorization
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` cmafPackage
      `Prelude.hashWithSalt` dashPackage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hlsPackage
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` manifestName
      `Prelude.hashWithSalt` mssPackage
      `Prelude.hashWithSalt` origination
      `Prelude.hashWithSalt` startoverWindowSeconds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeDelaySeconds
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` whitelist

instance Prelude.NFData OriginEndpoint where
  rnf OriginEndpoint' {..} =
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
