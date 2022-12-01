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
-- Module      : Amazonka.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DeltaTimeSessionWindowConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration information of a delta time
-- session window.
--
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_DeltaTime.html DeltaTime>
-- specifies a time interval. You can use @DeltaTime@ to create dataset
-- contents with data that has arrived in the data store since the last
-- execution. For an example of @DeltaTime@, see
-- <https://docs.aws.amazon.com/iotanalytics/latest/userguide/automate-create-dataset.html#automate-example6 Creating a SQL dataset with a delta window (CLI)>
-- in the /IoT Analytics User Guide/.
--
-- /See:/ 'newDeltaTimeSessionWindowConfiguration' smart constructor.
data DeltaTimeSessionWindowConfiguration = DeltaTimeSessionWindowConfiguration'
  { -- | A time interval. You can use @timeoutInMinutes@ so that IoT Analytics
    -- can batch up late data notifications that have been generated since the
    -- last execution. IoT Analytics sends one batch of notifications to Amazon
    -- CloudWatch Events at one time.
    --
    -- For more information about how to write a timestamp expression, see
    -- <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators>,
    -- in the /Presto 0.172 Documentation/.
    timeoutInMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeltaTimeSessionWindowConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutInMinutes', 'deltaTimeSessionWindowConfiguration_timeoutInMinutes' - A time interval. You can use @timeoutInMinutes@ so that IoT Analytics
-- can batch up late data notifications that have been generated since the
-- last execution. IoT Analytics sends one batch of notifications to Amazon
-- CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see
-- <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators>,
-- in the /Presto 0.172 Documentation/.
newDeltaTimeSessionWindowConfiguration ::
  -- | 'timeoutInMinutes'
  Prelude.Natural ->
  DeltaTimeSessionWindowConfiguration
newDeltaTimeSessionWindowConfiguration
  pTimeoutInMinutes_ =
    DeltaTimeSessionWindowConfiguration'
      { timeoutInMinutes =
          pTimeoutInMinutes_
      }

-- | A time interval. You can use @timeoutInMinutes@ so that IoT Analytics
-- can batch up late data notifications that have been generated since the
-- last execution. IoT Analytics sends one batch of notifications to Amazon
-- CloudWatch Events at one time.
--
-- For more information about how to write a timestamp expression, see
-- <https://prestodb.io/docs/0.172/functions/datetime.html Date and Time Functions and Operators>,
-- in the /Presto 0.172 Documentation/.
deltaTimeSessionWindowConfiguration_timeoutInMinutes :: Lens.Lens' DeltaTimeSessionWindowConfiguration Prelude.Natural
deltaTimeSessionWindowConfiguration_timeoutInMinutes = Lens.lens (\DeltaTimeSessionWindowConfiguration' {timeoutInMinutes} -> timeoutInMinutes) (\s@DeltaTimeSessionWindowConfiguration' {} a -> s {timeoutInMinutes = a} :: DeltaTimeSessionWindowConfiguration)

instance
  Core.FromJSON
    DeltaTimeSessionWindowConfiguration
  where
  parseJSON =
    Core.withObject
      "DeltaTimeSessionWindowConfiguration"
      ( \x ->
          DeltaTimeSessionWindowConfiguration'
            Prelude.<$> (x Core..: "timeoutInMinutes")
      )

instance
  Prelude.Hashable
    DeltaTimeSessionWindowConfiguration
  where
  hashWithSalt
    _salt
    DeltaTimeSessionWindowConfiguration' {..} =
      _salt `Prelude.hashWithSalt` timeoutInMinutes

instance
  Prelude.NFData
    DeltaTimeSessionWindowConfiguration
  where
  rnf DeltaTimeSessionWindowConfiguration' {..} =
    Prelude.rnf timeoutInMinutes

instance
  Core.ToJSON
    DeltaTimeSessionWindowConfiguration
  where
  toJSON DeltaTimeSessionWindowConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("timeoutInMinutes" Core..= timeoutInMinutes)
          ]
      )
