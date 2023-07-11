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
-- Module      : Amazonka.ElasticTranscoder.Types.Clip
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Clip where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.TimeSpan
import qualified Amazonka.Prelude as Prelude

-- | Settings for one clip in a composition. All jobs in a playlist must have
-- the same clip settings.
--
-- /See:/ 'newClip' smart constructor.
data Clip = Clip'
  { -- | Settings that determine when a clip begins and how long it lasts.
    timeSpan :: Prelude.Maybe TimeSpan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Clip where
  parseJSON =
    Data.withObject
      "Clip"
      (\x -> Clip' Prelude.<$> (x Data..:? "TimeSpan"))

instance Prelude.Hashable Clip where
  hashWithSalt _salt Clip' {..} =
    _salt `Prelude.hashWithSalt` timeSpan

instance Prelude.NFData Clip where
  rnf Clip' {..} = Prelude.rnf timeSpan

instance Data.ToJSON Clip where
  toJSON Clip' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TimeSpan" Data..=) Prelude.<$> timeSpan]
      )
