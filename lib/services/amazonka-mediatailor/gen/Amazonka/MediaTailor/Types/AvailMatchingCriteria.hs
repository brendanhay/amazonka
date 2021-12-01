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
-- Module      : Amazonka.MediaTailor.Types.AvailMatchingCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AvailMatchingCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.Operator
import qualified Amazonka.Prelude as Prelude

-- | MediaTailor only places (consumes) prefetched ads if the ad break meets
-- the criteria defined by the dynamic variables. This gives you granular
-- control over which ad break to place the prefetched ads into.
--
-- As an example, let\'s say that you set DynamicVariable to scte.event_id
-- and Operator to EQUALS, and your playback configuration has an ADS URL
-- of
-- https:\/\/my.ads.server.com\/path?&podId=[scte.avail_num]&event=[scte.event_id]&duration=[session.avail_duration_secs].
-- And the prefetch request to the ADS contains these values
-- https:\/\/my.ads.server.com\/path?&podId=3&event=my-awesome-event&duration=30.
-- MediaTailor will only insert the prefetched ads into the ad break if has
-- a SCTE marker with an event id of my-awesome-event, since it must match
-- the event id that MediaTailor uses to query the ADS.
--
-- You can specify up to five AvailMatchingCriteria. If you specify
-- multiple AvailMatchingCriteria, MediaTailor combines them to match using
-- a logical AND. You can model logical OR combinations by creating
-- multiple prefetch schedules.
--
-- /See:/ 'newAvailMatchingCriteria' smart constructor.
data AvailMatchingCriteria = AvailMatchingCriteria'
  { -- | For the DynamicVariable specified in AvailMatchingCriteria, the Operator
    -- that is used for the comparison.
    operator :: Operator,
    -- | The dynamic variable(s) that MediaTailor should use as avail matching
    -- criteria. MediaTailor only places the prefetched ads into the avail if
    -- the avail matches the criteria defined by the dynamic variable. For
    -- information about dynamic variables, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables.html Using dynamic ad variables>
    -- in the /MediaTailor User Guide/.
    --
    -- You can include up to 100 dynamic variables.
    dynamicVariable :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailMatchingCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'availMatchingCriteria_operator' - For the DynamicVariable specified in AvailMatchingCriteria, the Operator
-- that is used for the comparison.
--
-- 'dynamicVariable', 'availMatchingCriteria_dynamicVariable' - The dynamic variable(s) that MediaTailor should use as avail matching
-- criteria. MediaTailor only places the prefetched ads into the avail if
-- the avail matches the criteria defined by the dynamic variable. For
-- information about dynamic variables, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables.html Using dynamic ad variables>
-- in the /MediaTailor User Guide/.
--
-- You can include up to 100 dynamic variables.
newAvailMatchingCriteria ::
  -- | 'operator'
  Operator ->
  -- | 'dynamicVariable'
  Prelude.Text ->
  AvailMatchingCriteria
newAvailMatchingCriteria pOperator_ pDynamicVariable_ =
  AvailMatchingCriteria'
    { operator = pOperator_,
      dynamicVariable = pDynamicVariable_
    }

-- | For the DynamicVariable specified in AvailMatchingCriteria, the Operator
-- that is used for the comparison.
availMatchingCriteria_operator :: Lens.Lens' AvailMatchingCriteria Operator
availMatchingCriteria_operator = Lens.lens (\AvailMatchingCriteria' {operator} -> operator) (\s@AvailMatchingCriteria' {} a -> s {operator = a} :: AvailMatchingCriteria)

-- | The dynamic variable(s) that MediaTailor should use as avail matching
-- criteria. MediaTailor only places the prefetched ads into the avail if
-- the avail matches the criteria defined by the dynamic variable. For
-- information about dynamic variables, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables.html Using dynamic ad variables>
-- in the /MediaTailor User Guide/.
--
-- You can include up to 100 dynamic variables.
availMatchingCriteria_dynamicVariable :: Lens.Lens' AvailMatchingCriteria Prelude.Text
availMatchingCriteria_dynamicVariable = Lens.lens (\AvailMatchingCriteria' {dynamicVariable} -> dynamicVariable) (\s@AvailMatchingCriteria' {} a -> s {dynamicVariable = a} :: AvailMatchingCriteria)

instance Core.FromJSON AvailMatchingCriteria where
  parseJSON =
    Core.withObject
      "AvailMatchingCriteria"
      ( \x ->
          AvailMatchingCriteria'
            Prelude.<$> (x Core..: "Operator")
            Prelude.<*> (x Core..: "DynamicVariable")
      )

instance Prelude.Hashable AvailMatchingCriteria where
  hashWithSalt salt' AvailMatchingCriteria' {..} =
    salt' `Prelude.hashWithSalt` dynamicVariable
      `Prelude.hashWithSalt` operator

instance Prelude.NFData AvailMatchingCriteria where
  rnf AvailMatchingCriteria' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf dynamicVariable

instance Core.ToJSON AvailMatchingCriteria where
  toJSON AvailMatchingCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Operator" Core..= operator),
            Prelude.Just
              ("DynamicVariable" Core..= dynamicVariable)
          ]
      )
