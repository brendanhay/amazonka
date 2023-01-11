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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AvailMatchingCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.Operator
import qualified Amazonka.Prelude as Prelude

-- | MediaTailor only places (consumes) prefetched ads if the ad break meets
-- the criteria defined by the dynamic variables. This gives you granular
-- control over which ad break to place the prefetched ads into.
--
-- As an example, let\'s say that you set @DynamicVariable@ to
-- @scte.event_id@ and @Operator@ to @EQUALS@, and your playback
-- configuration has an ADS URL of
-- @https:\/\/my.ads.server.com\/path?&podId=[scte.avail_num]&event=[scte.event_id]&duration=[session.avail_duration_secs]@.
-- And the prefetch request to the ADS contains these values
-- @https:\/\/my.ads.server.com\/path?&podId=3&event=my-awesome-event&duration=30@.
-- MediaTailor will only insert the prefetched ads into the ad break if has
-- a SCTE marker with an event id of @my-awesome-event@, since it must
-- match the event id that MediaTailor uses to query the ADS.
--
-- You can specify up to five @AvailMatchingCriteria@. If you specify
-- multiple @AvailMatchingCriteria@, MediaTailor combines them to match
-- using a logical @AND@. You can model logical @OR@ combinations by
-- creating multiple prefetch schedules.
--
-- /See:/ 'newAvailMatchingCriteria' smart constructor.
data AvailMatchingCriteria = AvailMatchingCriteria'
  { -- | The dynamic variable(s) that MediaTailor should use as avail matching
    -- criteria. MediaTailor only places the prefetched ads into the avail if
    -- the avail matches the criteria defined by the dynamic variable. For
    -- information about dynamic variables, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables.html Using dynamic ad variables>
    -- in the /MediaTailor User Guide/.
    --
    -- You can include up to 100 dynamic variables.
    dynamicVariable :: Prelude.Text,
    -- | For the @DynamicVariable@ specified in @AvailMatchingCriteria@, the
    -- Operator that is used for the comparison.
    operator :: Operator
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
-- 'dynamicVariable', 'availMatchingCriteria_dynamicVariable' - The dynamic variable(s) that MediaTailor should use as avail matching
-- criteria. MediaTailor only places the prefetched ads into the avail if
-- the avail matches the criteria defined by the dynamic variable. For
-- information about dynamic variables, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/variables.html Using dynamic ad variables>
-- in the /MediaTailor User Guide/.
--
-- You can include up to 100 dynamic variables.
--
-- 'operator', 'availMatchingCriteria_operator' - For the @DynamicVariable@ specified in @AvailMatchingCriteria@, the
-- Operator that is used for the comparison.
newAvailMatchingCriteria ::
  -- | 'dynamicVariable'
  Prelude.Text ->
  -- | 'operator'
  Operator ->
  AvailMatchingCriteria
newAvailMatchingCriteria pDynamicVariable_ pOperator_ =
  AvailMatchingCriteria'
    { dynamicVariable =
        pDynamicVariable_,
      operator = pOperator_
    }

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

-- | For the @DynamicVariable@ specified in @AvailMatchingCriteria@, the
-- Operator that is used for the comparison.
availMatchingCriteria_operator :: Lens.Lens' AvailMatchingCriteria Operator
availMatchingCriteria_operator = Lens.lens (\AvailMatchingCriteria' {operator} -> operator) (\s@AvailMatchingCriteria' {} a -> s {operator = a} :: AvailMatchingCriteria)

instance Data.FromJSON AvailMatchingCriteria where
  parseJSON =
    Data.withObject
      "AvailMatchingCriteria"
      ( \x ->
          AvailMatchingCriteria'
            Prelude.<$> (x Data..: "DynamicVariable")
            Prelude.<*> (x Data..: "Operator")
      )

instance Prelude.Hashable AvailMatchingCriteria where
  hashWithSalt _salt AvailMatchingCriteria' {..} =
    _salt `Prelude.hashWithSalt` dynamicVariable
      `Prelude.hashWithSalt` operator

instance Prelude.NFData AvailMatchingCriteria where
  rnf AvailMatchingCriteria' {..} =
    Prelude.rnf dynamicVariable
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON AvailMatchingCriteria where
  toJSON AvailMatchingCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DynamicVariable" Data..= dynamicVariable),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
