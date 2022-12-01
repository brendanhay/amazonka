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
-- Module      : Amazonka.Wisdom.Types.QueryRecommendationTriggerData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.QueryRecommendationTriggerData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Data associated with the QUERY RecommendationTriggerType.
--
-- /See:/ 'newQueryRecommendationTriggerData' smart constructor.
data QueryRecommendationTriggerData = QueryRecommendationTriggerData'
  { -- | The text associated with the recommendation trigger.
    text :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryRecommendationTriggerData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'queryRecommendationTriggerData_text' - The text associated with the recommendation trigger.
newQueryRecommendationTriggerData ::
  QueryRecommendationTriggerData
newQueryRecommendationTriggerData =
  QueryRecommendationTriggerData'
    { text =
        Prelude.Nothing
    }

-- | The text associated with the recommendation trigger.
queryRecommendationTriggerData_text :: Lens.Lens' QueryRecommendationTriggerData (Prelude.Maybe Prelude.Text)
queryRecommendationTriggerData_text = Lens.lens (\QueryRecommendationTriggerData' {text} -> text) (\s@QueryRecommendationTriggerData' {} a -> s {text = a} :: QueryRecommendationTriggerData) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON QueryRecommendationTriggerData where
  parseJSON =
    Core.withObject
      "QueryRecommendationTriggerData"
      ( \x ->
          QueryRecommendationTriggerData'
            Prelude.<$> (x Core..:? "text")
      )

instance
  Prelude.Hashable
    QueryRecommendationTriggerData
  where
  hashWithSalt
    _salt
    QueryRecommendationTriggerData' {..} =
      _salt `Prelude.hashWithSalt` text

instance
  Prelude.NFData
    QueryRecommendationTriggerData
  where
  rnf QueryRecommendationTriggerData' {..} =
    Prelude.rnf text
