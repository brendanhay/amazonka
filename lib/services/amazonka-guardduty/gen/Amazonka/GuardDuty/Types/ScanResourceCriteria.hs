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
-- Module      : Amazonka.GuardDuty.Types.ScanResourceCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanResourceCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ScanCondition
import Amazonka.GuardDuty.Types.ScanCriterionKey
import qualified Amazonka.Prelude as Prelude

-- | Contains information about criteria used to filter resources before
-- triggering malware scan.
--
-- /See:/ 'newScanResourceCriteria' smart constructor.
data ScanResourceCriteria = ScanResourceCriteria'
  { -- | Represents condition that when matched will prevent a malware scan for a
    -- certain resource.
    exclude :: Prelude.Maybe (Prelude.HashMap ScanCriterionKey ScanCondition),
    -- | Represents condition that when matched will allow a malware scan for a
    -- certain resource.
    include :: Prelude.Maybe (Prelude.HashMap ScanCriterionKey ScanCondition)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanResourceCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclude', 'scanResourceCriteria_exclude' - Represents condition that when matched will prevent a malware scan for a
-- certain resource.
--
-- 'include', 'scanResourceCriteria_include' - Represents condition that when matched will allow a malware scan for a
-- certain resource.
newScanResourceCriteria ::
  ScanResourceCriteria
newScanResourceCriteria =
  ScanResourceCriteria'
    { exclude = Prelude.Nothing,
      include = Prelude.Nothing
    }

-- | Represents condition that when matched will prevent a malware scan for a
-- certain resource.
scanResourceCriteria_exclude :: Lens.Lens' ScanResourceCriteria (Prelude.Maybe (Prelude.HashMap ScanCriterionKey ScanCondition))
scanResourceCriteria_exclude = Lens.lens (\ScanResourceCriteria' {exclude} -> exclude) (\s@ScanResourceCriteria' {} a -> s {exclude = a} :: ScanResourceCriteria) Prelude.. Lens.mapping Lens.coerced

-- | Represents condition that when matched will allow a malware scan for a
-- certain resource.
scanResourceCriteria_include :: Lens.Lens' ScanResourceCriteria (Prelude.Maybe (Prelude.HashMap ScanCriterionKey ScanCondition))
scanResourceCriteria_include = Lens.lens (\ScanResourceCriteria' {include} -> include) (\s@ScanResourceCriteria' {} a -> s {include = a} :: ScanResourceCriteria) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ScanResourceCriteria where
  parseJSON =
    Data.withObject
      "ScanResourceCriteria"
      ( \x ->
          ScanResourceCriteria'
            Prelude.<$> (x Data..:? "exclude" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "include" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ScanResourceCriteria where
  hashWithSalt _salt ScanResourceCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` exclude
      `Prelude.hashWithSalt` include

instance Prelude.NFData ScanResourceCriteria where
  rnf ScanResourceCriteria' {..} =
    Prelude.rnf exclude
      `Prelude.seq` Prelude.rnf include

instance Data.ToJSON ScanResourceCriteria where
  toJSON ScanResourceCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exclude" Data..=) Prelude.<$> exclude,
            ("include" Data..=) Prelude.<$> include
          ]
      )
