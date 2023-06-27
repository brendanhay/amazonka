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
-- Module      : Amazonka.EMR.Types.SpotResizingSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SpotResizingSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resize specification for Spot Instances in the instance fleet, which
-- contains the resize timeout period.
--
-- /See:/ 'newSpotResizingSpecification' smart constructor.
data SpotResizingSpecification = SpotResizingSpecification'
  { -- | Spot resize timeout in minutes. If Spot Instances are not provisioned
    -- within this time, the resize workflow will stop provisioning of Spot
    -- instances. Minimum value is 5 minutes and maximum value is 10,080
    -- minutes (7 days). The timeout applies to all resize workflows on the
    -- Instance Fleet. The resize could be triggered by Amazon EMR Managed
    -- Scaling or by the customer (via Amazon EMR Console, Amazon EMR CLI
    -- modify-instance-fleet or Amazon EMR SDK ModifyInstanceFleet API) or by
    -- Amazon EMR due to Amazon EC2 Spot Reclamation.
    timeoutDurationMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotResizingSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutDurationMinutes', 'spotResizingSpecification_timeoutDurationMinutes' - Spot resize timeout in minutes. If Spot Instances are not provisioned
-- within this time, the resize workflow will stop provisioning of Spot
-- instances. Minimum value is 5 minutes and maximum value is 10,080
-- minutes (7 days). The timeout applies to all resize workflows on the
-- Instance Fleet. The resize could be triggered by Amazon EMR Managed
-- Scaling or by the customer (via Amazon EMR Console, Amazon EMR CLI
-- modify-instance-fleet or Amazon EMR SDK ModifyInstanceFleet API) or by
-- Amazon EMR due to Amazon EC2 Spot Reclamation.
newSpotResizingSpecification ::
  -- | 'timeoutDurationMinutes'
  Prelude.Natural ->
  SpotResizingSpecification
newSpotResizingSpecification pTimeoutDurationMinutes_ =
  SpotResizingSpecification'
    { timeoutDurationMinutes =
        pTimeoutDurationMinutes_
    }

-- | Spot resize timeout in minutes. If Spot Instances are not provisioned
-- within this time, the resize workflow will stop provisioning of Spot
-- instances. Minimum value is 5 minutes and maximum value is 10,080
-- minutes (7 days). The timeout applies to all resize workflows on the
-- Instance Fleet. The resize could be triggered by Amazon EMR Managed
-- Scaling or by the customer (via Amazon EMR Console, Amazon EMR CLI
-- modify-instance-fleet or Amazon EMR SDK ModifyInstanceFleet API) or by
-- Amazon EMR due to Amazon EC2 Spot Reclamation.
spotResizingSpecification_timeoutDurationMinutes :: Lens.Lens' SpotResizingSpecification Prelude.Natural
spotResizingSpecification_timeoutDurationMinutes = Lens.lens (\SpotResizingSpecification' {timeoutDurationMinutes} -> timeoutDurationMinutes) (\s@SpotResizingSpecification' {} a -> s {timeoutDurationMinutes = a} :: SpotResizingSpecification)

instance Data.FromJSON SpotResizingSpecification where
  parseJSON =
    Data.withObject
      "SpotResizingSpecification"
      ( \x ->
          SpotResizingSpecification'
            Prelude.<$> (x Data..: "TimeoutDurationMinutes")
      )

instance Prelude.Hashable SpotResizingSpecification where
  hashWithSalt _salt SpotResizingSpecification' {..} =
    _salt `Prelude.hashWithSalt` timeoutDurationMinutes

instance Prelude.NFData SpotResizingSpecification where
  rnf SpotResizingSpecification' {..} =
    Prelude.rnf timeoutDurationMinutes

instance Data.ToJSON SpotResizingSpecification where
  toJSON SpotResizingSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TimeoutDurationMinutes"
                  Data..= timeoutDurationMinutes
              )
          ]
      )
