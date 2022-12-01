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
-- Module      : Amazonka.SSMIncidents.Types.Integration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.Integration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.PagerDutyConfiguration

-- | Information about third-party services integrated into a response plan.
--
-- /See:/ 'newIntegration' smart constructor.
data Integration = Integration'
  { -- | Information about the PagerDuty service where the response plan creates
    -- an incident.
    pagerDutyConfiguration :: Prelude.Maybe PagerDutyConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Integration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pagerDutyConfiguration', 'integration_pagerDutyConfiguration' - Information about the PagerDuty service where the response plan creates
-- an incident.
newIntegration ::
  Integration
newIntegration =
  Integration'
    { pagerDutyConfiguration =
        Prelude.Nothing
    }

-- | Information about the PagerDuty service where the response plan creates
-- an incident.
integration_pagerDutyConfiguration :: Lens.Lens' Integration (Prelude.Maybe PagerDutyConfiguration)
integration_pagerDutyConfiguration = Lens.lens (\Integration' {pagerDutyConfiguration} -> pagerDutyConfiguration) (\s@Integration' {} a -> s {pagerDutyConfiguration = a} :: Integration)

instance Core.FromJSON Integration where
  parseJSON =
    Core.withObject
      "Integration"
      ( \x ->
          Integration'
            Prelude.<$> (x Core..:? "pagerDutyConfiguration")
      )

instance Prelude.Hashable Integration where
  hashWithSalt _salt Integration' {..} =
    _salt `Prelude.hashWithSalt` pagerDutyConfiguration

instance Prelude.NFData Integration where
  rnf Integration' {..} =
    Prelude.rnf pagerDutyConfiguration

instance Core.ToJSON Integration where
  toJSON Integration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pagerDutyConfiguration" Core..=)
              Prelude.<$> pagerDutyConfiguration
          ]
      )
