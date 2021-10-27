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
-- Module      : Network.AWS.DevOpsGuru.Types.ListInsightsAnyStatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.ListInsightsAnyStatusFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.InsightType
import Network.AWS.DevOpsGuru.Types.StartTimeRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable ListInsightsAnyStatusFilter

instance Prelude.NFData ListInsightsAnyStatusFilter

instance Core.ToJSON ListInsightsAnyStatusFilter where
  toJSON ListInsightsAnyStatusFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just
              ("StartTimeRange" Core..= startTimeRange)
          ]
      )
