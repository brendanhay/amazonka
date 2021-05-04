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
-- Module      : Network.AWS.MediaLive.Types.Scte27SourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte27SourceSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Scte27 Source Settings
--
-- /See:/ 'newScte27SourceSettings' smart constructor.
data Scte27SourceSettings = Scte27SourceSettings'
  { -- | The pid field is used in conjunction with the caption selector
    -- languageCode field as follows: - Specify PID and Language: Extracts
    -- captions from that PID; the language is \"informational\". - Specify PID
    -- and omit Language: Extracts the specified PID. - Omit PID and specify
    -- Language: Extracts the specified language, whichever PID that happens to
    -- be. - Omit PID and omit Language: Valid only if source is DVB-Sub that
    -- is being passed through; all languages will be passed through.
    pid :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte27SourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pid', 'scte27SourceSettings_pid' - The pid field is used in conjunction with the caption selector
-- languageCode field as follows: - Specify PID and Language: Extracts
-- captions from that PID; the language is \"informational\". - Specify PID
-- and omit Language: Extracts the specified PID. - Omit PID and specify
-- Language: Extracts the specified language, whichever PID that happens to
-- be. - Omit PID and omit Language: Valid only if source is DVB-Sub that
-- is being passed through; all languages will be passed through.
newScte27SourceSettings ::
  Scte27SourceSettings
newScte27SourceSettings =
  Scte27SourceSettings' {pid = Prelude.Nothing}

-- | The pid field is used in conjunction with the caption selector
-- languageCode field as follows: - Specify PID and Language: Extracts
-- captions from that PID; the language is \"informational\". - Specify PID
-- and omit Language: Extracts the specified PID. - Omit PID and specify
-- Language: Extracts the specified language, whichever PID that happens to
-- be. - Omit PID and omit Language: Valid only if source is DVB-Sub that
-- is being passed through; all languages will be passed through.
scte27SourceSettings_pid :: Lens.Lens' Scte27SourceSettings (Prelude.Maybe Prelude.Natural)
scte27SourceSettings_pid = Lens.lens (\Scte27SourceSettings' {pid} -> pid) (\s@Scte27SourceSettings' {} a -> s {pid = a} :: Scte27SourceSettings)

instance Prelude.FromJSON Scte27SourceSettings where
  parseJSON =
    Prelude.withObject
      "Scte27SourceSettings"
      ( \x ->
          Scte27SourceSettings'
            Prelude.<$> (x Prelude..:? "pid")
      )

instance Prelude.Hashable Scte27SourceSettings

instance Prelude.NFData Scte27SourceSettings

instance Prelude.ToJSON Scte27SourceSettings where
  toJSON Scte27SourceSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pid" Prelude..=) Prelude.<$> pid]
      )
