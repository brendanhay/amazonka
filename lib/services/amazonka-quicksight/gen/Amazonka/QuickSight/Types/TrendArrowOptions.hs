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
-- Module      : Amazonka.QuickSight.Types.TrendArrowOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TrendArrowOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the presentation of trend arrows in a KPI
-- visual.
--
-- /See:/ 'newTrendArrowOptions' smart constructor.
data TrendArrowOptions = TrendArrowOptions'
  { -- | The visibility of the trend arrows.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrendArrowOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'trendArrowOptions_visibility' - The visibility of the trend arrows.
newTrendArrowOptions ::
  TrendArrowOptions
newTrendArrowOptions =
  TrendArrowOptions' {visibility = Prelude.Nothing}

-- | The visibility of the trend arrows.
trendArrowOptions_visibility :: Lens.Lens' TrendArrowOptions (Prelude.Maybe Visibility)
trendArrowOptions_visibility = Lens.lens (\TrendArrowOptions' {visibility} -> visibility) (\s@TrendArrowOptions' {} a -> s {visibility = a} :: TrendArrowOptions)

instance Data.FromJSON TrendArrowOptions where
  parseJSON =
    Data.withObject
      "TrendArrowOptions"
      ( \x ->
          TrendArrowOptions'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable TrendArrowOptions where
  hashWithSalt _salt TrendArrowOptions' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData TrendArrowOptions where
  rnf TrendArrowOptions' {..} = Prelude.rnf visibility

instance Data.ToJSON TrendArrowOptions where
  toJSON TrendArrowOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
