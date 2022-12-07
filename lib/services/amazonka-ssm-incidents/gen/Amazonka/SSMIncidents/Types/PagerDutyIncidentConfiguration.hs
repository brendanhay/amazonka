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
-- Module      : Amazonka.SSMIncidents.Types.PagerDutyIncidentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.PagerDutyIncidentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the PagerDuty service where the response plan creates an
-- incident.
--
-- /See:/ 'newPagerDutyIncidentConfiguration' smart constructor.
data PagerDutyIncidentConfiguration = PagerDutyIncidentConfiguration'
  { -- | The ID of the PagerDuty service that the response plan associates with
    -- an incident when it launches.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PagerDutyIncidentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceId', 'pagerDutyIncidentConfiguration_serviceId' - The ID of the PagerDuty service that the response plan associates with
-- an incident when it launches.
newPagerDutyIncidentConfiguration ::
  -- | 'serviceId'
  Prelude.Text ->
  PagerDutyIncidentConfiguration
newPagerDutyIncidentConfiguration pServiceId_ =
  PagerDutyIncidentConfiguration'
    { serviceId =
        pServiceId_
    }

-- | The ID of the PagerDuty service that the response plan associates with
-- an incident when it launches.
pagerDutyIncidentConfiguration_serviceId :: Lens.Lens' PagerDutyIncidentConfiguration Prelude.Text
pagerDutyIncidentConfiguration_serviceId = Lens.lens (\PagerDutyIncidentConfiguration' {serviceId} -> serviceId) (\s@PagerDutyIncidentConfiguration' {} a -> s {serviceId = a} :: PagerDutyIncidentConfiguration)

instance Data.FromJSON PagerDutyIncidentConfiguration where
  parseJSON =
    Data.withObject
      "PagerDutyIncidentConfiguration"
      ( \x ->
          PagerDutyIncidentConfiguration'
            Prelude.<$> (x Data..: "serviceId")
      )

instance
  Prelude.Hashable
    PagerDutyIncidentConfiguration
  where
  hashWithSalt
    _salt
    PagerDutyIncidentConfiguration' {..} =
      _salt `Prelude.hashWithSalt` serviceId

instance
  Prelude.NFData
    PagerDutyIncidentConfiguration
  where
  rnf PagerDutyIncidentConfiguration' {..} =
    Prelude.rnf serviceId

instance Data.ToJSON PagerDutyIncidentConfiguration where
  toJSON PagerDutyIncidentConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("serviceId" Data..= serviceId)]
      )
