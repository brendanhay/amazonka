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
-- Module      : Amazonka.IoTFleetWise.Types.TimeBasedCollectionScheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.TimeBasedCollectionScheme where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a collection scheme that uses a time period to decide
-- how often to collect data.
--
-- /See:/ 'newTimeBasedCollectionScheme' smart constructor.
data TimeBasedCollectionScheme = TimeBasedCollectionScheme'
  { -- | The time period (in milliseconds) to decide how often to collect data.
    -- For example, if the time period is @60000@, the Edge Agent software
    -- collects data once every minute.
    periodMs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeBasedCollectionScheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'periodMs', 'timeBasedCollectionScheme_periodMs' - The time period (in milliseconds) to decide how often to collect data.
-- For example, if the time period is @60000@, the Edge Agent software
-- collects data once every minute.
newTimeBasedCollectionScheme ::
  -- | 'periodMs'
  Prelude.Natural ->
  TimeBasedCollectionScheme
newTimeBasedCollectionScheme pPeriodMs_ =
  TimeBasedCollectionScheme' {periodMs = pPeriodMs_}

-- | The time period (in milliseconds) to decide how often to collect data.
-- For example, if the time period is @60000@, the Edge Agent software
-- collects data once every minute.
timeBasedCollectionScheme_periodMs :: Lens.Lens' TimeBasedCollectionScheme Prelude.Natural
timeBasedCollectionScheme_periodMs = Lens.lens (\TimeBasedCollectionScheme' {periodMs} -> periodMs) (\s@TimeBasedCollectionScheme' {} a -> s {periodMs = a} :: TimeBasedCollectionScheme)

instance Data.FromJSON TimeBasedCollectionScheme where
  parseJSON =
    Data.withObject
      "TimeBasedCollectionScheme"
      ( \x ->
          TimeBasedCollectionScheme'
            Prelude.<$> (x Data..: "periodMs")
      )

instance Prelude.Hashable TimeBasedCollectionScheme where
  hashWithSalt _salt TimeBasedCollectionScheme' {..} =
    _salt `Prelude.hashWithSalt` periodMs

instance Prelude.NFData TimeBasedCollectionScheme where
  rnf TimeBasedCollectionScheme' {..} =
    Prelude.rnf periodMs

instance Data.ToJSON TimeBasedCollectionScheme where
  toJSON TimeBasedCollectionScheme' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("periodMs" Data..= periodMs)]
      )
