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
-- Module      : Amazonka.Inspector2.Types.Remediation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Remediation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.Recommendation
import qualified Amazonka.Prelude as Prelude

-- | Information on how to remediate a finding.
--
-- /See:/ 'newRemediation' smart constructor.
data Remediation = Remediation'
  { -- | An object that contains information about the recommended course of
    -- action to remediate the finding.
    recommendation :: Prelude.Maybe Recommendation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Remediation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendation', 'remediation_recommendation' - An object that contains information about the recommended course of
-- action to remediate the finding.
newRemediation ::
  Remediation
newRemediation =
  Remediation' {recommendation = Prelude.Nothing}

-- | An object that contains information about the recommended course of
-- action to remediate the finding.
remediation_recommendation :: Lens.Lens' Remediation (Prelude.Maybe Recommendation)
remediation_recommendation = Lens.lens (\Remediation' {recommendation} -> recommendation) (\s@Remediation' {} a -> s {recommendation = a} :: Remediation)

instance Core.FromJSON Remediation where
  parseJSON =
    Core.withObject
      "Remediation"
      ( \x ->
          Remediation'
            Prelude.<$> (x Core..:? "recommendation")
      )

instance Prelude.Hashable Remediation where
  hashWithSalt _salt Remediation' {..} =
    _salt `Prelude.hashWithSalt` recommendation

instance Prelude.NFData Remediation where
  rnf Remediation' {..} = Prelude.rnf recommendation
