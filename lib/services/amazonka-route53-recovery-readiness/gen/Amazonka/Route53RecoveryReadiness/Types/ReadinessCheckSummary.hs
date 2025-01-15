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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.ReadinessCheckSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.Readiness

-- | Summary of all readiness check statuses in a recovery group, paginated
-- in GetRecoveryGroupReadinessSummary and GetCellReadinessSummary.
--
-- /See:/ 'newReadinessCheckSummary' smart constructor.
data ReadinessCheckSummary = ReadinessCheckSummary'
  { -- | The readiness status of this readiness check.
    readiness :: Prelude.Maybe Readiness,
    -- | The name of a readiness check.
    readinessCheckName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadinessCheckSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readiness', 'readinessCheckSummary_readiness' - The readiness status of this readiness check.
--
-- 'readinessCheckName', 'readinessCheckSummary_readinessCheckName' - The name of a readiness check.
newReadinessCheckSummary ::
  ReadinessCheckSummary
newReadinessCheckSummary =
  ReadinessCheckSummary'
    { readiness = Prelude.Nothing,
      readinessCheckName = Prelude.Nothing
    }

-- | The readiness status of this readiness check.
readinessCheckSummary_readiness :: Lens.Lens' ReadinessCheckSummary (Prelude.Maybe Readiness)
readinessCheckSummary_readiness = Lens.lens (\ReadinessCheckSummary' {readiness} -> readiness) (\s@ReadinessCheckSummary' {} a -> s {readiness = a} :: ReadinessCheckSummary)

-- | The name of a readiness check.
readinessCheckSummary_readinessCheckName :: Lens.Lens' ReadinessCheckSummary (Prelude.Maybe Prelude.Text)
readinessCheckSummary_readinessCheckName = Lens.lens (\ReadinessCheckSummary' {readinessCheckName} -> readinessCheckName) (\s@ReadinessCheckSummary' {} a -> s {readinessCheckName = a} :: ReadinessCheckSummary)

instance Data.FromJSON ReadinessCheckSummary where
  parseJSON =
    Data.withObject
      "ReadinessCheckSummary"
      ( \x ->
          ReadinessCheckSummary'
            Prelude.<$> (x Data..:? "readiness")
            Prelude.<*> (x Data..:? "readinessCheckName")
      )

instance Prelude.Hashable ReadinessCheckSummary where
  hashWithSalt _salt ReadinessCheckSummary' {..} =
    _salt
      `Prelude.hashWithSalt` readiness
      `Prelude.hashWithSalt` readinessCheckName

instance Prelude.NFData ReadinessCheckSummary where
  rnf ReadinessCheckSummary' {..} =
    Prelude.rnf readiness `Prelude.seq`
      Prelude.rnf readinessCheckName
