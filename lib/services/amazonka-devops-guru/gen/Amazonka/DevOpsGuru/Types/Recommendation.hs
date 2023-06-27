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
-- Module      : Amazonka.DevOpsGuru.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.RecommendationRelatedAnomaly
import Amazonka.DevOpsGuru.Types.RecommendationRelatedEvent
import qualified Amazonka.Prelude as Prelude

-- | Recommendation information to help you remediate detected anomalous
-- behavior that generated an insight.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The category type of the recommendation.
    category :: Prelude.Maybe Prelude.Text,
    -- | A description of the problem.
    description :: Prelude.Maybe Prelude.Text,
    -- | A hyperlink to information to help you address the problem.
    link :: Prelude.Maybe Prelude.Text,
    -- | The name of the recommendation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The reason DevOps Guru flagged the anomalous behavior as a problem.
    reason :: Prelude.Maybe Prelude.Text,
    -- | Anomalies that are related to the problem. Use these Anomalies to learn
    -- more about what\'s happening and to help address the issue.
    relatedAnomalies :: Prelude.Maybe [RecommendationRelatedAnomaly],
    -- | Events that are related to the problem. Use these events to learn more
    -- about what\'s happening and to help address the issue.
    relatedEvents :: Prelude.Maybe [RecommendationRelatedEvent]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'recommendation_category' - The category type of the recommendation.
--
-- 'description', 'recommendation_description' - A description of the problem.
--
-- 'link', 'recommendation_link' - A hyperlink to information to help you address the problem.
--
-- 'name', 'recommendation_name' - The name of the recommendation.
--
-- 'reason', 'recommendation_reason' - The reason DevOps Guru flagged the anomalous behavior as a problem.
--
-- 'relatedAnomalies', 'recommendation_relatedAnomalies' - Anomalies that are related to the problem. Use these Anomalies to learn
-- more about what\'s happening and to help address the issue.
--
-- 'relatedEvents', 'recommendation_relatedEvents' - Events that are related to the problem. Use these events to learn more
-- about what\'s happening and to help address the issue.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { category = Prelude.Nothing,
      description = Prelude.Nothing,
      link = Prelude.Nothing,
      name = Prelude.Nothing,
      reason = Prelude.Nothing,
      relatedAnomalies = Prelude.Nothing,
      relatedEvents = Prelude.Nothing
    }

-- | The category type of the recommendation.
recommendation_category :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_category = Lens.lens (\Recommendation' {category} -> category) (\s@Recommendation' {} a -> s {category = a} :: Recommendation)

-- | A description of the problem.
recommendation_description :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_description = Lens.lens (\Recommendation' {description} -> description) (\s@Recommendation' {} a -> s {description = a} :: Recommendation)

-- | A hyperlink to information to help you address the problem.
recommendation_link :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_link = Lens.lens (\Recommendation' {link} -> link) (\s@Recommendation' {} a -> s {link = a} :: Recommendation)

-- | The name of the recommendation.
recommendation_name :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_name = Lens.lens (\Recommendation' {name} -> name) (\s@Recommendation' {} a -> s {name = a} :: Recommendation)

-- | The reason DevOps Guru flagged the anomalous behavior as a problem.
recommendation_reason :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_reason = Lens.lens (\Recommendation' {reason} -> reason) (\s@Recommendation' {} a -> s {reason = a} :: Recommendation)

-- | Anomalies that are related to the problem. Use these Anomalies to learn
-- more about what\'s happening and to help address the issue.
recommendation_relatedAnomalies :: Lens.Lens' Recommendation (Prelude.Maybe [RecommendationRelatedAnomaly])
recommendation_relatedAnomalies = Lens.lens (\Recommendation' {relatedAnomalies} -> relatedAnomalies) (\s@Recommendation' {} a -> s {relatedAnomalies = a} :: Recommendation) Prelude.. Lens.mapping Lens.coerced

-- | Events that are related to the problem. Use these events to learn more
-- about what\'s happening and to help address the issue.
recommendation_relatedEvents :: Lens.Lens' Recommendation (Prelude.Maybe [RecommendationRelatedEvent])
recommendation_relatedEvents = Lens.lens (\Recommendation' {relatedEvents} -> relatedEvents) (\s@Recommendation' {} a -> s {relatedEvents = a} :: Recommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..:? "Category")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Link")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> ( x
                            Data..:? "RelatedAnomalies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RelatedEvents" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` link
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` relatedAnomalies
      `Prelude.hashWithSalt` relatedEvents

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf link
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf relatedAnomalies
      `Prelude.seq` Prelude.rnf relatedEvents
