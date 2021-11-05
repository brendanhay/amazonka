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
-- Module      : Network.AWS.Synthetics.Types.CanaryTimeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Synthetics.Types.CanaryTimeline where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure contains information about when the canary was created
-- and modified.
--
-- /See:/ 'newCanaryTimeline' smart constructor.
data CanaryTimeline = CanaryTimeline'
  { -- | The date and time the canary was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the canary\'s most recent run started.
    lastStarted :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the canary\'s most recent run ended.
    lastStopped :: Prelude.Maybe Core.POSIX,
    -- | The date and time the canary was most recently modified.
    lastModified :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'canaryTimeline_created' - The date and time the canary was created.
--
-- 'lastStarted', 'canaryTimeline_lastStarted' - The date and time that the canary\'s most recent run started.
--
-- 'lastStopped', 'canaryTimeline_lastStopped' - The date and time that the canary\'s most recent run ended.
--
-- 'lastModified', 'canaryTimeline_lastModified' - The date and time the canary was most recently modified.
newCanaryTimeline ::
  CanaryTimeline
newCanaryTimeline =
  CanaryTimeline'
    { created = Prelude.Nothing,
      lastStarted = Prelude.Nothing,
      lastStopped = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | The date and time the canary was created.
canaryTimeline_created :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_created = Lens.lens (\CanaryTimeline' {created} -> created) (\s@CanaryTimeline' {} a -> s {created = a} :: CanaryTimeline) Prelude.. Lens.mapping Core._Time

-- | The date and time that the canary\'s most recent run started.
canaryTimeline_lastStarted :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastStarted = Lens.lens (\CanaryTimeline' {lastStarted} -> lastStarted) (\s@CanaryTimeline' {} a -> s {lastStarted = a} :: CanaryTimeline) Prelude.. Lens.mapping Core._Time

-- | The date and time that the canary\'s most recent run ended.
canaryTimeline_lastStopped :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastStopped = Lens.lens (\CanaryTimeline' {lastStopped} -> lastStopped) (\s@CanaryTimeline' {} a -> s {lastStopped = a} :: CanaryTimeline) Prelude.. Lens.mapping Core._Time

-- | The date and time the canary was most recently modified.
canaryTimeline_lastModified :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastModified = Lens.lens (\CanaryTimeline' {lastModified} -> lastModified) (\s@CanaryTimeline' {} a -> s {lastModified = a} :: CanaryTimeline) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CanaryTimeline where
  parseJSON =
    Core.withObject
      "CanaryTimeline"
      ( \x ->
          CanaryTimeline'
            Prelude.<$> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "LastStarted")
            Prelude.<*> (x Core..:? "LastStopped")
            Prelude.<*> (x Core..:? "LastModified")
      )

instance Prelude.Hashable CanaryTimeline

instance Prelude.NFData CanaryTimeline
