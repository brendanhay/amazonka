{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.DateInterval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DateInterval where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The time period of the request.
--
-- /See:/ 'newDateInterval' smart constructor.
data DateInterval = DateInterval'
  { -- | The beginning of the time period. The start date is inclusive. For
    -- example, if @start@ is @2017-01-01@, AWS retrieves cost and usage data
    -- starting at @2017-01-01@ up to the end date. The start date must be
    -- equal to or no later than the current date to avoid a validation error.
    start :: Prelude.Text,
    -- | The end of the time period. The end date is exclusive. For example, if
    -- @end@ is @2017-05-01@, AWS retrieves cost and usage data from the start
    -- date up to, but not including, @2017-05-01@.
    end :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DateInterval' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'dateInterval_start' - The beginning of the time period. The start date is inclusive. For
-- example, if @start@ is @2017-01-01@, AWS retrieves cost and usage data
-- starting at @2017-01-01@ up to the end date. The start date must be
-- equal to or no later than the current date to avoid a validation error.
--
-- 'end', 'dateInterval_end' - The end of the time period. The end date is exclusive. For example, if
-- @end@ is @2017-05-01@, AWS retrieves cost and usage data from the start
-- date up to, but not including, @2017-05-01@.
newDateInterval ::
  -- | 'start'
  Prelude.Text ->
  -- | 'end'
  Prelude.Text ->
  DateInterval
newDateInterval pStart_ pEnd_ =
  DateInterval' {start = pStart_, end = pEnd_}

-- | The beginning of the time period. The start date is inclusive. For
-- example, if @start@ is @2017-01-01@, AWS retrieves cost and usage data
-- starting at @2017-01-01@ up to the end date. The start date must be
-- equal to or no later than the current date to avoid a validation error.
dateInterval_start :: Lens.Lens' DateInterval Prelude.Text
dateInterval_start = Lens.lens (\DateInterval' {start} -> start) (\s@DateInterval' {} a -> s {start = a} :: DateInterval)

-- | The end of the time period. The end date is exclusive. For example, if
-- @end@ is @2017-05-01@, AWS retrieves cost and usage data from the start
-- date up to, but not including, @2017-05-01@.
dateInterval_end :: Lens.Lens' DateInterval Prelude.Text
dateInterval_end = Lens.lens (\DateInterval' {end} -> end) (\s@DateInterval' {} a -> s {end = a} :: DateInterval)

instance Prelude.FromJSON DateInterval where
  parseJSON =
    Prelude.withObject
      "DateInterval"
      ( \x ->
          DateInterval'
            Prelude.<$> (x Prelude..: "Start")
            Prelude.<*> (x Prelude..: "End")
      )

instance Prelude.Hashable DateInterval

instance Prelude.NFData DateInterval

instance Prelude.ToJSON DateInterval where
  toJSON DateInterval' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Start" Prelude..= start),
            Prelude.Just ("End" Prelude..= end)
          ]
      )
