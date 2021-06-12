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
-- Module      : Network.AWS.ElasticTranscoder.Types.Clip
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Clip where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types.TimeSpan
import qualified Network.AWS.Lens as Lens

-- | Settings for one clip in a composition. All jobs in a playlist must have
-- the same clip settings.
--
-- /See:/ 'newClip' smart constructor.
data Clip = Clip'
  { -- | Settings that determine when a clip begins and how long it lasts.
    timeSpan :: Core.Maybe TimeSpan
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Clip' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeSpan', 'clip_timeSpan' - Settings that determine when a clip begins and how long it lasts.
newClip ::
  Clip
newClip = Clip' {timeSpan = Core.Nothing}

-- | Settings that determine when a clip begins and how long it lasts.
clip_timeSpan :: Lens.Lens' Clip (Core.Maybe TimeSpan)
clip_timeSpan = Lens.lens (\Clip' {timeSpan} -> timeSpan) (\s@Clip' {} a -> s {timeSpan = a} :: Clip)

instance Core.FromJSON Clip where
  parseJSON =
    Core.withObject
      "Clip"
      (\x -> Clip' Core.<$> (x Core..:? "TimeSpan"))

instance Core.Hashable Clip

instance Core.NFData Clip

instance Core.ToJSON Clip where
  toJSON Clip' {..} =
    Core.object
      ( Core.catMaybes
          [("TimeSpan" Core..=) Core.<$> timeSpan]
      )
