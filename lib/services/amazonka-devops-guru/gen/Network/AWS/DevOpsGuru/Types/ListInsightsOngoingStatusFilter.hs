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
-- Module      : Network.AWS.DevOpsGuru.Types.ListInsightsOngoingStatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.ListInsightsOngoingStatusFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.InsightType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to filter for insights that have the status @ONGOING@.
--
-- /See:/ 'newListInsightsOngoingStatusFilter' smart constructor.
data ListInsightsOngoingStatusFilter = ListInsightsOngoingStatusFilter'
  { -- | Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
    type' :: InsightType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInsightsOngoingStatusFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'listInsightsOngoingStatusFilter_type' - Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
newListInsightsOngoingStatusFilter ::
  -- | 'type''
  InsightType ->
  ListInsightsOngoingStatusFilter
newListInsightsOngoingStatusFilter pType_ =
  ListInsightsOngoingStatusFilter' {type' = pType_}

-- | Use to filter for either @REACTIVE@ or @PROACTIVE@ insights.
listInsightsOngoingStatusFilter_type :: Lens.Lens' ListInsightsOngoingStatusFilter InsightType
listInsightsOngoingStatusFilter_type = Lens.lens (\ListInsightsOngoingStatusFilter' {type'} -> type') (\s@ListInsightsOngoingStatusFilter' {} a -> s {type' = a} :: ListInsightsOngoingStatusFilter)

instance
  Prelude.Hashable
    ListInsightsOngoingStatusFilter

instance
  Prelude.NFData
    ListInsightsOngoingStatusFilter

instance Core.ToJSON ListInsightsOngoingStatusFilter where
  toJSON ListInsightsOngoingStatusFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Core..= type')]
      )
