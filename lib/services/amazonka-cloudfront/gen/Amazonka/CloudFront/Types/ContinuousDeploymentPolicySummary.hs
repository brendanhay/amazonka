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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentPolicySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentPolicySummary where

import Amazonka.CloudFront.Types.ContinuousDeploymentPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the information about your continuous deployment policies.
--
-- /See:/ 'newContinuousDeploymentPolicySummary' smart constructor.
data ContinuousDeploymentPolicySummary = ContinuousDeploymentPolicySummary'
  { -- | The continuous deployment policy.
    continuousDeploymentPolicy :: ContinuousDeploymentPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuousDeploymentPolicy', 'continuousDeploymentPolicySummary_continuousDeploymentPolicy' - The continuous deployment policy.
newContinuousDeploymentPolicySummary ::
  -- | 'continuousDeploymentPolicy'
  ContinuousDeploymentPolicy ->
  ContinuousDeploymentPolicySummary
newContinuousDeploymentPolicySummary
  pContinuousDeploymentPolicy_ =
    ContinuousDeploymentPolicySummary'
      { continuousDeploymentPolicy =
          pContinuousDeploymentPolicy_
      }

-- | The continuous deployment policy.
continuousDeploymentPolicySummary_continuousDeploymentPolicy :: Lens.Lens' ContinuousDeploymentPolicySummary ContinuousDeploymentPolicy
continuousDeploymentPolicySummary_continuousDeploymentPolicy = Lens.lens (\ContinuousDeploymentPolicySummary' {continuousDeploymentPolicy} -> continuousDeploymentPolicy) (\s@ContinuousDeploymentPolicySummary' {} a -> s {continuousDeploymentPolicy = a} :: ContinuousDeploymentPolicySummary)

instance
  Data.FromXML
    ContinuousDeploymentPolicySummary
  where
  parseXML x =
    ContinuousDeploymentPolicySummary'
      Prelude.<$> (x Data..@ "ContinuousDeploymentPolicy")

instance
  Prelude.Hashable
    ContinuousDeploymentPolicySummary
  where
  hashWithSalt
    _salt
    ContinuousDeploymentPolicySummary' {..} =
      _salt
        `Prelude.hashWithSalt` continuousDeploymentPolicy

instance
  Prelude.NFData
    ContinuousDeploymentPolicySummary
  where
  rnf ContinuousDeploymentPolicySummary' {..} =
    Prelude.rnf continuousDeploymentPolicy
