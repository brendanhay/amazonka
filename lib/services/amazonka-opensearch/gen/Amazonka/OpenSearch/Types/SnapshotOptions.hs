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
-- Module      : Amazonka.OpenSearch.Types.SnapshotOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SnapshotOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time, in UTC format, when OpenSearch Service takes a daily automated
-- snapshot of the specified domain. Default is @0@ hours.
--
-- /See:/ 'newSnapshotOptions' smart constructor.
data SnapshotOptions = SnapshotOptions'
  { -- | The time, in UTC format, when OpenSearch Service takes a daily automated
    -- snapshot of the specified domain. Default is @0@ hours.
    automatedSnapshotStartHour :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automatedSnapshotStartHour', 'snapshotOptions_automatedSnapshotStartHour' - The time, in UTC format, when OpenSearch Service takes a daily automated
-- snapshot of the specified domain. Default is @0@ hours.
newSnapshotOptions ::
  SnapshotOptions
newSnapshotOptions =
  SnapshotOptions'
    { automatedSnapshotStartHour =
        Prelude.Nothing
    }

-- | The time, in UTC format, when OpenSearch Service takes a daily automated
-- snapshot of the specified domain. Default is @0@ hours.
snapshotOptions_automatedSnapshotStartHour :: Lens.Lens' SnapshotOptions (Prelude.Maybe Prelude.Int)
snapshotOptions_automatedSnapshotStartHour = Lens.lens (\SnapshotOptions' {automatedSnapshotStartHour} -> automatedSnapshotStartHour) (\s@SnapshotOptions' {} a -> s {automatedSnapshotStartHour = a} :: SnapshotOptions)

instance Core.FromJSON SnapshotOptions where
  parseJSON =
    Core.withObject
      "SnapshotOptions"
      ( \x ->
          SnapshotOptions'
            Prelude.<$> (x Core..:? "AutomatedSnapshotStartHour")
      )

instance Prelude.Hashable SnapshotOptions where
  hashWithSalt _salt SnapshotOptions' {..} =
    _salt
      `Prelude.hashWithSalt` automatedSnapshotStartHour

instance Prelude.NFData SnapshotOptions where
  rnf SnapshotOptions' {..} =
    Prelude.rnf automatedSnapshotStartHour

instance Core.ToJSON SnapshotOptions where
  toJSON SnapshotOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutomatedSnapshotStartHour" Core..=)
              Prelude.<$> automatedSnapshotStartHour
          ]
      )
