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
-- Module      : Amazonka.IoTSiteWise.Types.RetentionPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.RetentionPeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | How many days your data is kept in the hot tier. By default, your data
-- is kept indefinitely in the hot tier.
--
-- /See:/ 'newRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { -- | The number of days that your data is kept.
    --
    -- If you specified a value for this parameter, the @unlimited@ parameter
    -- must be @false@.
    numberOfDays :: Prelude.Maybe Prelude.Natural,
    -- | If true, your data is kept indefinitely.
    --
    -- If configured to @true@, you must not specify a value for the
    -- @numberOfDays@ parameter.
    unlimited :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfDays', 'retentionPeriod_numberOfDays' - The number of days that your data is kept.
--
-- If you specified a value for this parameter, the @unlimited@ parameter
-- must be @false@.
--
-- 'unlimited', 'retentionPeriod_unlimited' - If true, your data is kept indefinitely.
--
-- If configured to @true@, you must not specify a value for the
-- @numberOfDays@ parameter.
newRetentionPeriod ::
  RetentionPeriod
newRetentionPeriod =
  RetentionPeriod'
    { numberOfDays = Prelude.Nothing,
      unlimited = Prelude.Nothing
    }

-- | The number of days that your data is kept.
--
-- If you specified a value for this parameter, the @unlimited@ parameter
-- must be @false@.
retentionPeriod_numberOfDays :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Natural)
retentionPeriod_numberOfDays = Lens.lens (\RetentionPeriod' {numberOfDays} -> numberOfDays) (\s@RetentionPeriod' {} a -> s {numberOfDays = a} :: RetentionPeriod)

-- | If true, your data is kept indefinitely.
--
-- If configured to @true@, you must not specify a value for the
-- @numberOfDays@ parameter.
retentionPeriod_unlimited :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Bool)
retentionPeriod_unlimited = Lens.lens (\RetentionPeriod' {unlimited} -> unlimited) (\s@RetentionPeriod' {} a -> s {unlimited = a} :: RetentionPeriod)

instance Data.FromJSON RetentionPeriod where
  parseJSON =
    Data.withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            Prelude.<$> (x Data..:? "numberOfDays")
            Prelude.<*> (x Data..:? "unlimited")
      )

instance Prelude.Hashable RetentionPeriod where
  hashWithSalt _salt RetentionPeriod' {..} =
    _salt
      `Prelude.hashWithSalt` numberOfDays
      `Prelude.hashWithSalt` unlimited

instance Prelude.NFData RetentionPeriod where
  rnf RetentionPeriod' {..} =
    Prelude.rnf numberOfDays `Prelude.seq`
      Prelude.rnf unlimited

instance Data.ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("numberOfDays" Data..=) Prelude.<$> numberOfDays,
            ("unlimited" Data..=) Prelude.<$> unlimited
          ]
      )
