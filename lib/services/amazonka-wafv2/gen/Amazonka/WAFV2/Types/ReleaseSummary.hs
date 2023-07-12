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
-- Module      : Amazonka.WAFV2.Types.ReleaseSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ReleaseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | High level information for an SDK release.
--
-- /See:/ 'newReleaseSummary' smart constructor.
data ReleaseSummary = ReleaseSummary'
  { -- | The release version.
    releaseVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the release.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleaseSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseVersion', 'releaseSummary_releaseVersion' - The release version.
--
-- 'timestamp', 'releaseSummary_timestamp' - The timestamp of the release.
newReleaseSummary ::
  ReleaseSummary
newReleaseSummary =
  ReleaseSummary'
    { releaseVersion = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The release version.
releaseSummary_releaseVersion :: Lens.Lens' ReleaseSummary (Prelude.Maybe Prelude.Text)
releaseSummary_releaseVersion = Lens.lens (\ReleaseSummary' {releaseVersion} -> releaseVersion) (\s@ReleaseSummary' {} a -> s {releaseVersion = a} :: ReleaseSummary)

-- | The timestamp of the release.
releaseSummary_timestamp :: Lens.Lens' ReleaseSummary (Prelude.Maybe Prelude.UTCTime)
releaseSummary_timestamp = Lens.lens (\ReleaseSummary' {timestamp} -> timestamp) (\s@ReleaseSummary' {} a -> s {timestamp = a} :: ReleaseSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ReleaseSummary where
  parseJSON =
    Data.withObject
      "ReleaseSummary"
      ( \x ->
          ReleaseSummary'
            Prelude.<$> (x Data..:? "ReleaseVersion")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable ReleaseSummary where
  hashWithSalt _salt ReleaseSummary' {..} =
    _salt
      `Prelude.hashWithSalt` releaseVersion
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData ReleaseSummary where
  rnf ReleaseSummary' {..} =
    Prelude.rnf releaseVersion
      `Prelude.seq` Prelude.rnf timestamp
