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
-- Module      : Amazonka.Synthetics.Types.CanaryTimeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about when the canary was created
-- and modified.
--
-- /See:/ 'newCanaryTimeline' smart constructor.
data CanaryTimeline = CanaryTimeline'
  { -- | The date and time the canary was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The date and time the canary was most recently modified.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the canary\'s most recent run started.
    lastStarted :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the canary\'s most recent run ended.
    lastStopped :: Prelude.Maybe Data.POSIX
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
-- 'lastModified', 'canaryTimeline_lastModified' - The date and time the canary was most recently modified.
--
-- 'lastStarted', 'canaryTimeline_lastStarted' - The date and time that the canary\'s most recent run started.
--
-- 'lastStopped', 'canaryTimeline_lastStopped' - The date and time that the canary\'s most recent run ended.
newCanaryTimeline ::
  CanaryTimeline
newCanaryTimeline =
  CanaryTimeline'
    { created = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      lastStarted = Prelude.Nothing,
      lastStopped = Prelude.Nothing
    }

-- | The date and time the canary was created.
canaryTimeline_created :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_created = Lens.lens (\CanaryTimeline' {created} -> created) (\s@CanaryTimeline' {} a -> s {created = a} :: CanaryTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time the canary was most recently modified.
canaryTimeline_lastModified :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastModified = Lens.lens (\CanaryTimeline' {lastModified} -> lastModified) (\s@CanaryTimeline' {} a -> s {lastModified = a} :: CanaryTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time that the canary\'s most recent run started.
canaryTimeline_lastStarted :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastStarted = Lens.lens (\CanaryTimeline' {lastStarted} -> lastStarted) (\s@CanaryTimeline' {} a -> s {lastStarted = a} :: CanaryTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time that the canary\'s most recent run ended.
canaryTimeline_lastStopped :: Lens.Lens' CanaryTimeline (Prelude.Maybe Prelude.UTCTime)
canaryTimeline_lastStopped = Lens.lens (\CanaryTimeline' {lastStopped} -> lastStopped) (\s@CanaryTimeline' {} a -> s {lastStopped = a} :: CanaryTimeline) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CanaryTimeline where
  parseJSON =
    Data.withObject
      "CanaryTimeline"
      ( \x ->
          CanaryTimeline'
            Prelude.<$> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "LastStarted")
            Prelude.<*> (x Data..:? "LastStopped")
      )

instance Prelude.Hashable CanaryTimeline where
  hashWithSalt _salt CanaryTimeline' {..} =
    _salt
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` lastStarted
      `Prelude.hashWithSalt` lastStopped

instance Prelude.NFData CanaryTimeline where
  rnf CanaryTimeline' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf lastStarted
      `Prelude.seq` Prelude.rnf lastStopped
