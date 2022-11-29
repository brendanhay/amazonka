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
-- Module      : Amazonka.DMS.Types.FleetAdvisorLsaAnalysisResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.FleetAdvisorLsaAnalysisResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a large-scale assessment (LSA) analysis run by a Fleet Advisor
-- collector.
--
-- /See:/ 'newFleetAdvisorLsaAnalysisResponse' smart constructor.
data FleetAdvisorLsaAnalysisResponse = FleetAdvisorLsaAnalysisResponse'
  { -- | The ID of an LSA analysis run by a Fleet Advisor collector.
    lsaAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The status of an LSA analysis run by a Fleet Advisor collector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetAdvisorLsaAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lsaAnalysisId', 'fleetAdvisorLsaAnalysisResponse_lsaAnalysisId' - The ID of an LSA analysis run by a Fleet Advisor collector.
--
-- 'status', 'fleetAdvisorLsaAnalysisResponse_status' - The status of an LSA analysis run by a Fleet Advisor collector.
newFleetAdvisorLsaAnalysisResponse ::
  FleetAdvisorLsaAnalysisResponse
newFleetAdvisorLsaAnalysisResponse =
  FleetAdvisorLsaAnalysisResponse'
    { lsaAnalysisId =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID of an LSA analysis run by a Fleet Advisor collector.
fleetAdvisorLsaAnalysisResponse_lsaAnalysisId :: Lens.Lens' FleetAdvisorLsaAnalysisResponse (Prelude.Maybe Prelude.Text)
fleetAdvisorLsaAnalysisResponse_lsaAnalysisId = Lens.lens (\FleetAdvisorLsaAnalysisResponse' {lsaAnalysisId} -> lsaAnalysisId) (\s@FleetAdvisorLsaAnalysisResponse' {} a -> s {lsaAnalysisId = a} :: FleetAdvisorLsaAnalysisResponse)

-- | The status of an LSA analysis run by a Fleet Advisor collector.
fleetAdvisorLsaAnalysisResponse_status :: Lens.Lens' FleetAdvisorLsaAnalysisResponse (Prelude.Maybe Prelude.Text)
fleetAdvisorLsaAnalysisResponse_status = Lens.lens (\FleetAdvisorLsaAnalysisResponse' {status} -> status) (\s@FleetAdvisorLsaAnalysisResponse' {} a -> s {status = a} :: FleetAdvisorLsaAnalysisResponse)

instance
  Core.FromJSON
    FleetAdvisorLsaAnalysisResponse
  where
  parseJSON =
    Core.withObject
      "FleetAdvisorLsaAnalysisResponse"
      ( \x ->
          FleetAdvisorLsaAnalysisResponse'
            Prelude.<$> (x Core..:? "LsaAnalysisId")
            Prelude.<*> (x Core..:? "Status")
      )

instance
  Prelude.Hashable
    FleetAdvisorLsaAnalysisResponse
  where
  hashWithSalt
    _salt
    FleetAdvisorLsaAnalysisResponse' {..} =
      _salt `Prelude.hashWithSalt` lsaAnalysisId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    FleetAdvisorLsaAnalysisResponse
  where
  rnf FleetAdvisorLsaAnalysisResponse' {..} =
    Prelude.rnf lsaAnalysisId
      `Prelude.seq` Prelude.rnf status
