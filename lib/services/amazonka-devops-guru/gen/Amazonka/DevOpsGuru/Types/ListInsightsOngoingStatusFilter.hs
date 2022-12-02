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
-- Module      : Amazonka.DevOpsGuru.Types.ListInsightsOngoingStatusFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListInsightsOngoingStatusFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightType
import qualified Amazonka.Prelude as Prelude

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
  where
  hashWithSalt
    _salt
    ListInsightsOngoingStatusFilter' {..} =
      _salt `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    ListInsightsOngoingStatusFilter
  where
  rnf ListInsightsOngoingStatusFilter' {..} =
    Prelude.rnf type'

instance Data.ToJSON ListInsightsOngoingStatusFilter where
  toJSON ListInsightsOngoingStatusFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Data..= type')]
      )
