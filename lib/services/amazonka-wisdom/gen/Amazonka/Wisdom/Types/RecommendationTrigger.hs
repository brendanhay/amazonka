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
-- Module      : Amazonka.Wisdom.Types.RecommendationTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.RecommendationTrigger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.RecommendationSourceType
import Amazonka.Wisdom.Types.RecommendationTriggerData
import Amazonka.Wisdom.Types.RecommendationTriggerType

-- | A recommendation trigger provides context on the event that produced the
-- referenced recommendations. Recommendations are only referenced in
-- @recommendationIds@ by a single RecommendationTrigger.
--
-- /See:/ 'newRecommendationTrigger' smart constructor.
data RecommendationTrigger = RecommendationTrigger'
  { -- | A union type containing information related to the trigger.
    data' :: RecommendationTriggerData,
    -- | The identifier of the recommendation trigger.
    id :: Prelude.Text,
    -- | The identifiers of the recommendations.
    recommendationIds :: [Prelude.Text],
    -- | The source of the recommendation trigger.
    --
    -- -   ISSUE_DETECTION: The corresponding recommendations were triggered by
    --     a Contact Lens issue.
    --
    -- -   RULE_EVALUATION: The corresponding recommendations were triggered by
    --     a Contact Lens rule.
    source :: RecommendationSourceType,
    -- | The type of recommendation trigger.
    type' :: RecommendationTriggerType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'recommendationTrigger_data' - A union type containing information related to the trigger.
--
-- 'id', 'recommendationTrigger_id' - The identifier of the recommendation trigger.
--
-- 'recommendationIds', 'recommendationTrigger_recommendationIds' - The identifiers of the recommendations.
--
-- 'source', 'recommendationTrigger_source' - The source of the recommendation trigger.
--
-- -   ISSUE_DETECTION: The corresponding recommendations were triggered by
--     a Contact Lens issue.
--
-- -   RULE_EVALUATION: The corresponding recommendations were triggered by
--     a Contact Lens rule.
--
-- 'type'', 'recommendationTrigger_type' - The type of recommendation trigger.
newRecommendationTrigger ::
  -- | 'data''
  RecommendationTriggerData ->
  -- | 'id'
  Prelude.Text ->
  -- | 'source'
  RecommendationSourceType ->
  -- | 'type''
  RecommendationTriggerType ->
  RecommendationTrigger
newRecommendationTrigger pData_ pId_ pSource_ pType_ =
  RecommendationTrigger'
    { data' = pData_,
      id = pId_,
      recommendationIds = Prelude.mempty,
      source = pSource_,
      type' = pType_
    }

-- | A union type containing information related to the trigger.
recommendationTrigger_data :: Lens.Lens' RecommendationTrigger RecommendationTriggerData
recommendationTrigger_data = Lens.lens (\RecommendationTrigger' {data'} -> data') (\s@RecommendationTrigger' {} a -> s {data' = a} :: RecommendationTrigger)

-- | The identifier of the recommendation trigger.
recommendationTrigger_id :: Lens.Lens' RecommendationTrigger Prelude.Text
recommendationTrigger_id = Lens.lens (\RecommendationTrigger' {id} -> id) (\s@RecommendationTrigger' {} a -> s {id = a} :: RecommendationTrigger)

-- | The identifiers of the recommendations.
recommendationTrigger_recommendationIds :: Lens.Lens' RecommendationTrigger [Prelude.Text]
recommendationTrigger_recommendationIds = Lens.lens (\RecommendationTrigger' {recommendationIds} -> recommendationIds) (\s@RecommendationTrigger' {} a -> s {recommendationIds = a} :: RecommendationTrigger) Prelude.. Lens.coerced

-- | The source of the recommendation trigger.
--
-- -   ISSUE_DETECTION: The corresponding recommendations were triggered by
--     a Contact Lens issue.
--
-- -   RULE_EVALUATION: The corresponding recommendations were triggered by
--     a Contact Lens rule.
recommendationTrigger_source :: Lens.Lens' RecommendationTrigger RecommendationSourceType
recommendationTrigger_source = Lens.lens (\RecommendationTrigger' {source} -> source) (\s@RecommendationTrigger' {} a -> s {source = a} :: RecommendationTrigger)

-- | The type of recommendation trigger.
recommendationTrigger_type :: Lens.Lens' RecommendationTrigger RecommendationTriggerType
recommendationTrigger_type = Lens.lens (\RecommendationTrigger' {type'} -> type') (\s@RecommendationTrigger' {} a -> s {type' = a} :: RecommendationTrigger)

instance Data.FromJSON RecommendationTrigger where
  parseJSON =
    Data.withObject
      "RecommendationTrigger"
      ( \x ->
          RecommendationTrigger'
            Prelude.<$> (x Data..: "data")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> ( x
                            Data..:? "recommendationIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "source")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable RecommendationTrigger where
  hashWithSalt _salt RecommendationTrigger' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` recommendationIds
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RecommendationTrigger where
  rnf RecommendationTrigger' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf recommendationIds
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf type'
