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
-- Module      : Amazonka.GuardDuty.Types.ScanCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ScanCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ScanConditionPair
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the condition.
--
-- /See:/ 'newScanCondition' smart constructor.
data ScanCondition = ScanCondition'
  { -- | Represents an /mapEqual/ ____ condition to be applied to a single field
    -- when triggering for malware scan.
    mapEquals :: [ScanConditionPair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScanCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapEquals', 'scanCondition_mapEquals' - Represents an /mapEqual/ ____ condition to be applied to a single field
-- when triggering for malware scan.
newScanCondition ::
  ScanCondition
newScanCondition =
  ScanCondition' {mapEquals = Prelude.mempty}

-- | Represents an /mapEqual/ ____ condition to be applied to a single field
-- when triggering for malware scan.
scanCondition_mapEquals :: Lens.Lens' ScanCondition [ScanConditionPair]
scanCondition_mapEquals = Lens.lens (\ScanCondition' {mapEquals} -> mapEquals) (\s@ScanCondition' {} a -> s {mapEquals = a} :: ScanCondition) Prelude.. Lens.coerced

instance Data.FromJSON ScanCondition where
  parseJSON =
    Data.withObject
      "ScanCondition"
      ( \x ->
          ScanCondition'
            Prelude.<$> (x Data..:? "mapEquals" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ScanCondition where
  hashWithSalt _salt ScanCondition' {..} =
    _salt `Prelude.hashWithSalt` mapEquals

instance Prelude.NFData ScanCondition where
  rnf ScanCondition' {..} = Prelude.rnf mapEquals

instance Data.ToJSON ScanCondition where
  toJSON ScanCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("mapEquals" Data..= mapEquals)]
      )
