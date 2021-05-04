{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticTranscoder.Types.TimeSpan
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for one clip in a composition. All jobs in a playlist must have
-- the same clip settings.
--
-- /See:/ 'newClip' smart constructor.
data Clip = Clip'
  { -- | Settings that determine when a clip begins and how long it lasts.
    timeSpan :: Prelude.Maybe TimeSpan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newClip = Clip' {timeSpan = Prelude.Nothing}

-- | Settings that determine when a clip begins and how long it lasts.
clip_timeSpan :: Lens.Lens' Clip (Prelude.Maybe TimeSpan)
clip_timeSpan = Lens.lens (\Clip' {timeSpan} -> timeSpan) (\s@Clip' {} a -> s {timeSpan = a} :: Clip)

instance Prelude.FromJSON Clip where
  parseJSON =
    Prelude.withObject
      "Clip"
      (\x -> Clip' Prelude.<$> (x Prelude..:? "TimeSpan"))

instance Prelude.Hashable Clip

instance Prelude.NFData Clip

instance Prelude.ToJSON Clip where
  toJSON Clip' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("TimeSpan" Prelude..=) Prelude.<$> timeSpan]
      )
