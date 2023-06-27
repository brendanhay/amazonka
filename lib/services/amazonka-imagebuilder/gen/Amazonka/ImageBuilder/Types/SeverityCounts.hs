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
-- Module      : Amazonka.ImageBuilder.Types.SeverityCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.SeverityCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Includes counts by severity level for medium severity and higher level
-- findings, plus a total for all of the findings for the specified filter.
--
-- /See:/ 'newSeverityCounts' smart constructor.
data SeverityCounts = SeverityCounts'
  { -- | The total number of findings across all severity levels for the
    -- specified filter.
    all :: Prelude.Maybe Prelude.Integer,
    -- | The number of critical severity findings for the specified filter.
    critical :: Prelude.Maybe Prelude.Integer,
    -- | The number of high severity findings for the specified filter.
    high :: Prelude.Maybe Prelude.Integer,
    -- | The number of medium severity findings for the specified filter.
    medium :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SeverityCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'all', 'severityCounts_all' - The total number of findings across all severity levels for the
-- specified filter.
--
-- 'critical', 'severityCounts_critical' - The number of critical severity findings for the specified filter.
--
-- 'high', 'severityCounts_high' - The number of high severity findings for the specified filter.
--
-- 'medium', 'severityCounts_medium' - The number of medium severity findings for the specified filter.
newSeverityCounts ::
  SeverityCounts
newSeverityCounts =
  SeverityCounts'
    { all = Prelude.Nothing,
      critical = Prelude.Nothing,
      high = Prelude.Nothing,
      medium = Prelude.Nothing
    }

-- | The total number of findings across all severity levels for the
-- specified filter.
severityCounts_all :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_all = Lens.lens (\SeverityCounts' {all} -> all) (\s@SeverityCounts' {} a -> s {all = a} :: SeverityCounts)

-- | The number of critical severity findings for the specified filter.
severityCounts_critical :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_critical = Lens.lens (\SeverityCounts' {critical} -> critical) (\s@SeverityCounts' {} a -> s {critical = a} :: SeverityCounts)

-- | The number of high severity findings for the specified filter.
severityCounts_high :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_high = Lens.lens (\SeverityCounts' {high} -> high) (\s@SeverityCounts' {} a -> s {high = a} :: SeverityCounts)

-- | The number of medium severity findings for the specified filter.
severityCounts_medium :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_medium = Lens.lens (\SeverityCounts' {medium} -> medium) (\s@SeverityCounts' {} a -> s {medium = a} :: SeverityCounts)

instance Data.FromJSON SeverityCounts where
  parseJSON =
    Data.withObject
      "SeverityCounts"
      ( \x ->
          SeverityCounts'
            Prelude.<$> (x Data..:? "all")
            Prelude.<*> (x Data..:? "critical")
            Prelude.<*> (x Data..:? "high")
            Prelude.<*> (x Data..:? "medium")
      )

instance Prelude.Hashable SeverityCounts where
  hashWithSalt _salt SeverityCounts' {..} =
    _salt
      `Prelude.hashWithSalt` all
      `Prelude.hashWithSalt` critical
      `Prelude.hashWithSalt` high
      `Prelude.hashWithSalt` medium

instance Prelude.NFData SeverityCounts where
  rnf SeverityCounts' {..} =
    Prelude.rnf all
      `Prelude.seq` Prelude.rnf critical
      `Prelude.seq` Prelude.rnf high
      `Prelude.seq` Prelude.rnf medium
