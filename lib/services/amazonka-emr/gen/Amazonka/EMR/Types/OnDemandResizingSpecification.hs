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
-- Module      : Amazonka.EMR.Types.OnDemandResizingSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.OnDemandResizingSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resize specification for On-Demand Instances in the instance fleet,
-- which contains the resize timeout period.
--
-- /See:/ 'newOnDemandResizingSpecification' smart constructor.
data OnDemandResizingSpecification = OnDemandResizingSpecification'
  { -- | On-Demand resize timeout in minutes. If On-Demand Instances are not
    -- provisioned within this time, the resize workflow stops. The minimum
    -- value is 5 minutes, and the maximum value is 10,080 minutes (7 days).
    -- The timeout applies to all resize workflows on the Instance Fleet. The
    -- resize could be triggered by Amazon EMR Managed Scaling or by the
    -- customer (via Amazon EMR Console, Amazon EMR CLI modify-instance-fleet
    -- or Amazon EMR SDK ModifyInstanceFleet API) or by Amazon EMR due to
    -- Amazon EC2 Spot Reclamation.
    timeoutDurationMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDemandResizingSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutDurationMinutes', 'onDemandResizingSpecification_timeoutDurationMinutes' - On-Demand resize timeout in minutes. If On-Demand Instances are not
-- provisioned within this time, the resize workflow stops. The minimum
-- value is 5 minutes, and the maximum value is 10,080 minutes (7 days).
-- The timeout applies to all resize workflows on the Instance Fleet. The
-- resize could be triggered by Amazon EMR Managed Scaling or by the
-- customer (via Amazon EMR Console, Amazon EMR CLI modify-instance-fleet
-- or Amazon EMR SDK ModifyInstanceFleet API) or by Amazon EMR due to
-- Amazon EC2 Spot Reclamation.
newOnDemandResizingSpecification ::
  -- | 'timeoutDurationMinutes'
  Prelude.Natural ->
  OnDemandResizingSpecification
newOnDemandResizingSpecification
  pTimeoutDurationMinutes_ =
    OnDemandResizingSpecification'
      { timeoutDurationMinutes =
          pTimeoutDurationMinutes_
      }

-- | On-Demand resize timeout in minutes. If On-Demand Instances are not
-- provisioned within this time, the resize workflow stops. The minimum
-- value is 5 minutes, and the maximum value is 10,080 minutes (7 days).
-- The timeout applies to all resize workflows on the Instance Fleet. The
-- resize could be triggered by Amazon EMR Managed Scaling or by the
-- customer (via Amazon EMR Console, Amazon EMR CLI modify-instance-fleet
-- or Amazon EMR SDK ModifyInstanceFleet API) or by Amazon EMR due to
-- Amazon EC2 Spot Reclamation.
onDemandResizingSpecification_timeoutDurationMinutes :: Lens.Lens' OnDemandResizingSpecification Prelude.Natural
onDemandResizingSpecification_timeoutDurationMinutes = Lens.lens (\OnDemandResizingSpecification' {timeoutDurationMinutes} -> timeoutDurationMinutes) (\s@OnDemandResizingSpecification' {} a -> s {timeoutDurationMinutes = a} :: OnDemandResizingSpecification)

instance Data.FromJSON OnDemandResizingSpecification where
  parseJSON =
    Data.withObject
      "OnDemandResizingSpecification"
      ( \x ->
          OnDemandResizingSpecification'
            Prelude.<$> (x Data..: "TimeoutDurationMinutes")
      )

instance
  Prelude.Hashable
    OnDemandResizingSpecification
  where
  hashWithSalt _salt OnDemandResizingSpecification' {..} =
    _salt `Prelude.hashWithSalt` timeoutDurationMinutes

instance Prelude.NFData OnDemandResizingSpecification where
  rnf OnDemandResizingSpecification' {..} =
    Prelude.rnf timeoutDurationMinutes

instance Data.ToJSON OnDemandResizingSpecification where
  toJSON OnDemandResizingSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TimeoutDurationMinutes"
                  Data..= timeoutDurationMinutes
              )
          ]
      )
