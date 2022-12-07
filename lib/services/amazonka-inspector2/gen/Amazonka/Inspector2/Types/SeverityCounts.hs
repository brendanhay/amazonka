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
-- Module      : Amazonka.Inspector2.Types.SeverityCounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.SeverityCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the counts of aggregated finding per severity.
--
-- /See:/ 'newSeverityCounts' smart constructor.
data SeverityCounts = SeverityCounts'
  { -- | The total count of critical severity findings.
    critical :: Prelude.Maybe Prelude.Integer,
    -- | The total count of high severity findings.
    high :: Prelude.Maybe Prelude.Integer,
    -- | The total count of findings from all severities.
    all :: Prelude.Maybe Prelude.Integer,
    -- | The total count of medium severity findings.
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
-- 'critical', 'severityCounts_critical' - The total count of critical severity findings.
--
-- 'high', 'severityCounts_high' - The total count of high severity findings.
--
-- 'all', 'severityCounts_all' - The total count of findings from all severities.
--
-- 'medium', 'severityCounts_medium' - The total count of medium severity findings.
newSeverityCounts ::
  SeverityCounts
newSeverityCounts =
  SeverityCounts'
    { critical = Prelude.Nothing,
      high = Prelude.Nothing,
      all = Prelude.Nothing,
      medium = Prelude.Nothing
    }

-- | The total count of critical severity findings.
severityCounts_critical :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_critical = Lens.lens (\SeverityCounts' {critical} -> critical) (\s@SeverityCounts' {} a -> s {critical = a} :: SeverityCounts)

-- | The total count of high severity findings.
severityCounts_high :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_high = Lens.lens (\SeverityCounts' {high} -> high) (\s@SeverityCounts' {} a -> s {high = a} :: SeverityCounts)

-- | The total count of findings from all severities.
severityCounts_all :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_all = Lens.lens (\SeverityCounts' {all} -> all) (\s@SeverityCounts' {} a -> s {all = a} :: SeverityCounts)

-- | The total count of medium severity findings.
severityCounts_medium :: Lens.Lens' SeverityCounts (Prelude.Maybe Prelude.Integer)
severityCounts_medium = Lens.lens (\SeverityCounts' {medium} -> medium) (\s@SeverityCounts' {} a -> s {medium = a} :: SeverityCounts)

instance Data.FromJSON SeverityCounts where
  parseJSON =
    Data.withObject
      "SeverityCounts"
      ( \x ->
          SeverityCounts'
            Prelude.<$> (x Data..:? "critical")
            Prelude.<*> (x Data..:? "high")
            Prelude.<*> (x Data..:? "all")
            Prelude.<*> (x Data..:? "medium")
      )

instance Prelude.Hashable SeverityCounts where
  hashWithSalt _salt SeverityCounts' {..} =
    _salt `Prelude.hashWithSalt` critical
      `Prelude.hashWithSalt` high
      `Prelude.hashWithSalt` all
      `Prelude.hashWithSalt` medium

instance Prelude.NFData SeverityCounts where
  rnf SeverityCounts' {..} =
    Prelude.rnf critical
      `Prelude.seq` Prelude.rnf high
      `Prelude.seq` Prelude.rnf all
      `Prelude.seq` Prelude.rnf medium
