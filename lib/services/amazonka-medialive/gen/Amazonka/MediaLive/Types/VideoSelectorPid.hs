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
-- Module      : Amazonka.MediaLive.Types.VideoSelectorPid
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VideoSelectorPid where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Video Selector Pid
--
-- /See:/ 'newVideoSelectorPid' smart constructor.
data VideoSelectorPid = VideoSelectorPid'
  { -- | Selects a specific PID from within a video source.
    pid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelectorPid' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pid', 'videoSelectorPid_pid' - Selects a specific PID from within a video source.
newVideoSelectorPid ::
  VideoSelectorPid
newVideoSelectorPid =
  VideoSelectorPid' {pid = Prelude.Nothing}

-- | Selects a specific PID from within a video source.
videoSelectorPid_pid :: Lens.Lens' VideoSelectorPid (Prelude.Maybe Prelude.Natural)
videoSelectorPid_pid = Lens.lens (\VideoSelectorPid' {pid} -> pid) (\s@VideoSelectorPid' {} a -> s {pid = a} :: VideoSelectorPid)

instance Data.FromJSON VideoSelectorPid where
  parseJSON =
    Data.withObject
      "VideoSelectorPid"
      ( \x ->
          VideoSelectorPid' Prelude.<$> (x Data..:? "pid")
      )

instance Prelude.Hashable VideoSelectorPid where
  hashWithSalt _salt VideoSelectorPid' {..} =
    _salt `Prelude.hashWithSalt` pid

instance Prelude.NFData VideoSelectorPid where
  rnf VideoSelectorPid' {..} = Prelude.rnf pid

instance Data.ToJSON VideoSelectorPid where
  toJSON VideoSelectorPid' {..} =
    Data.object
      (Prelude.catMaybes [("pid" Data..=) Prelude.<$> pid])
