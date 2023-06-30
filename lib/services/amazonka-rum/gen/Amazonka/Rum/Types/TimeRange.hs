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
-- Module      : Amazonka.Rum.Types.TimeRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.TimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines the time range that you want to retrieve
-- results from.
--
-- /See:/ 'newTimeRange' smart constructor.
data TimeRange = TimeRange'
  { -- | The end of the time range to retrieve performance events from. If you
    -- omit this, the time range extends to the time that this operation is
    -- performed.
    before :: Prelude.Maybe Prelude.Integer,
    -- | The beginning of the time range to retrieve performance events from.
    after :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'before', 'timeRange_before' - The end of the time range to retrieve performance events from. If you
-- omit this, the time range extends to the time that this operation is
-- performed.
--
-- 'after', 'timeRange_after' - The beginning of the time range to retrieve performance events from.
newTimeRange ::
  -- | 'after'
  Prelude.Integer ->
  TimeRange
newTimeRange pAfter_ =
  TimeRange'
    { before = Prelude.Nothing,
      after = pAfter_
    }

-- | The end of the time range to retrieve performance events from. If you
-- omit this, the time range extends to the time that this operation is
-- performed.
timeRange_before :: Lens.Lens' TimeRange (Prelude.Maybe Prelude.Integer)
timeRange_before = Lens.lens (\TimeRange' {before} -> before) (\s@TimeRange' {} a -> s {before = a} :: TimeRange)

-- | The beginning of the time range to retrieve performance events from.
timeRange_after :: Lens.Lens' TimeRange Prelude.Integer
timeRange_after = Lens.lens (\TimeRange' {after} -> after) (\s@TimeRange' {} a -> s {after = a} :: TimeRange)

instance Prelude.Hashable TimeRange where
  hashWithSalt _salt TimeRange' {..} =
    _salt
      `Prelude.hashWithSalt` before
      `Prelude.hashWithSalt` after

instance Prelude.NFData TimeRange where
  rnf TimeRange' {..} =
    Prelude.rnf before `Prelude.seq` Prelude.rnf after

instance Data.ToJSON TimeRange where
  toJSON TimeRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Before" Data..=) Prelude.<$> before,
            Prelude.Just ("After" Data..= after)
          ]
      )
