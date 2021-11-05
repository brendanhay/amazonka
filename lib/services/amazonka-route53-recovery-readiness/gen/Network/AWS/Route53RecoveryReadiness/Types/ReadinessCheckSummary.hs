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
-- Module      : Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryReadiness.Types.ReadinessCheckSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryReadiness.Types.Readiness

-- | Summary of ReadinessCheck status, paginated in
-- GetRecoveryGroupReadinessSummary and GetCellReadinessSummary
--
-- /See:/ 'newReadinessCheckSummary' smart constructor.
data ReadinessCheckSummary = ReadinessCheckSummary'
  { -- | The readiness of this ReadinessCheck
    readiness :: Prelude.Maybe Readiness,
    -- | The name of a ReadinessCheck which is part of the given RecoveryGroup or
    -- Cell
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
-- 'readiness', 'readinessCheckSummary_readiness' - The readiness of this ReadinessCheck
--
-- 'readinessCheckName', 'readinessCheckSummary_readinessCheckName' - The name of a ReadinessCheck which is part of the given RecoveryGroup or
-- Cell
newReadinessCheckSummary ::
  ReadinessCheckSummary
newReadinessCheckSummary =
  ReadinessCheckSummary'
    { readiness = Prelude.Nothing,
      readinessCheckName = Prelude.Nothing
    }

-- | The readiness of this ReadinessCheck
readinessCheckSummary_readiness :: Lens.Lens' ReadinessCheckSummary (Prelude.Maybe Readiness)
readinessCheckSummary_readiness = Lens.lens (\ReadinessCheckSummary' {readiness} -> readiness) (\s@ReadinessCheckSummary' {} a -> s {readiness = a} :: ReadinessCheckSummary)

-- | The name of a ReadinessCheck which is part of the given RecoveryGroup or
-- Cell
readinessCheckSummary_readinessCheckName :: Lens.Lens' ReadinessCheckSummary (Prelude.Maybe Prelude.Text)
readinessCheckSummary_readinessCheckName = Lens.lens (\ReadinessCheckSummary' {readinessCheckName} -> readinessCheckName) (\s@ReadinessCheckSummary' {} a -> s {readinessCheckName = a} :: ReadinessCheckSummary)

instance Core.FromJSON ReadinessCheckSummary where
  parseJSON =
    Core.withObject
      "ReadinessCheckSummary"
      ( \x ->
          ReadinessCheckSummary'
            Prelude.<$> (x Core..:? "readiness")
            Prelude.<*> (x Core..:? "readinessCheckName")
      )

instance Prelude.Hashable ReadinessCheckSummary

instance Prelude.NFData ReadinessCheckSummary
