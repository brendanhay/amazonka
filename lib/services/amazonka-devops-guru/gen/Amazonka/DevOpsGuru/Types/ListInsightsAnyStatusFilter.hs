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
-- Module      : Amazonka.DevOpsGuru.Types.ListInsightsAnyStatusFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListInsightsAnyStatusFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.InsightType
import Amazonka.DevOpsGuru.Types.StartTimeRange
import qualified Amazonka.Prelude as Prelude

-- | Used to filter for insights that have any status.
--
-- /See:/ 'newListInsightsAnyStatusFilter' smart constructor.
data ListInsightsAnyStatusFilter = ListInsightsAnyStatusFilter'
  { -- | Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
    type' :: InsightType,
    -- | A time range used to specify when the behavior of the filtered insights
    -- started.
    startTimeRange :: StartTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInsightsAnyStatusFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'listInsightsAnyStatusFilter_type' - Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
--
-- 'startTimeRange', 'listInsightsAnyStatusFilter_startTimeRange' - A time range used to specify when the behavior of the filtered insights
-- started.
newListInsightsAnyStatusFilter ::
  -- | 'type''
  InsightType ->
  -- | 'startTimeRange'
  StartTimeRange ->
  ListInsightsAnyStatusFilter
newListInsightsAnyStatusFilter
  pType_
  pStartTimeRange_ =
    ListInsightsAnyStatusFilter'
      { type' = pType_,
        startTimeRange = pStartTimeRange_
      }

-- | Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
listInsightsAnyStatusFilter_type :: Lens.Lens' ListInsightsAnyStatusFilter InsightType
listInsightsAnyStatusFilter_type = Lens.lens (\ListInsightsAnyStatusFilter' {type'} -> type') (\s@ListInsightsAnyStatusFilter' {} a -> s {type' = a} :: ListInsightsAnyStatusFilter)

-- | A time range used to specify when the behavior of the filtered insights
-- started.
listInsightsAnyStatusFilter_startTimeRange :: Lens.Lens' ListInsightsAnyStatusFilter StartTimeRange
listInsightsAnyStatusFilter_startTimeRange = Lens.lens (\ListInsightsAnyStatusFilter' {startTimeRange} -> startTimeRange) (\s@ListInsightsAnyStatusFilter' {} a -> s {startTimeRange = a} :: ListInsightsAnyStatusFilter)

instance Prelude.Hashable ListInsightsAnyStatusFilter where
  hashWithSalt _salt ListInsightsAnyStatusFilter' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` startTimeRange

instance Prelude.NFData ListInsightsAnyStatusFilter where
  rnf ListInsightsAnyStatusFilter' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf startTimeRange

instance Core.ToJSON ListInsightsAnyStatusFilter where
  toJSON ListInsightsAnyStatusFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ("StartTimeRange" Core..= startTimeRange)
          ]
      )
