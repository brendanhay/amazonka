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
-- Module      : Amazonka.Wisdom.Types.RecommendationTriggerData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.RecommendationTriggerData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.QueryRecommendationTriggerData

-- | A union type containing information related to the trigger.
--
-- /See:/ 'newRecommendationTriggerData' smart constructor.
data RecommendationTriggerData = RecommendationTriggerData'
  { -- | Data associated with the QUERY RecommendationTriggerType.
    query :: Prelude.Maybe QueryRecommendationTriggerData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationTriggerData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'query', 'recommendationTriggerData_query' - Data associated with the QUERY RecommendationTriggerType.
newRecommendationTriggerData ::
  RecommendationTriggerData
newRecommendationTriggerData =
  RecommendationTriggerData' {query = Prelude.Nothing}

-- | Data associated with the QUERY RecommendationTriggerType.
recommendationTriggerData_query :: Lens.Lens' RecommendationTriggerData (Prelude.Maybe QueryRecommendationTriggerData)
recommendationTriggerData_query = Lens.lens (\RecommendationTriggerData' {query} -> query) (\s@RecommendationTriggerData' {} a -> s {query = a} :: RecommendationTriggerData)

instance Data.FromJSON RecommendationTriggerData where
  parseJSON =
    Data.withObject
      "RecommendationTriggerData"
      ( \x ->
          RecommendationTriggerData'
            Prelude.<$> (x Data..:? "query")
      )

instance Prelude.Hashable RecommendationTriggerData where
  hashWithSalt _salt RecommendationTriggerData' {..} =
    _salt `Prelude.hashWithSalt` query

instance Prelude.NFData RecommendationTriggerData where
  rnf RecommendationTriggerData' {..} =
    Prelude.rnf query
