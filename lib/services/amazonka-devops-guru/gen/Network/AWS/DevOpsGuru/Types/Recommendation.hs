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
-- Module      : Network.AWS.DevOpsGuru.Types.Recommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.Recommendation where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomaly
import Network.AWS.DevOpsGuru.Types.RecommendationRelatedEvent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Recommendation information to help you remediate detected anomalous
-- behavior that generated an insight.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | A hyperlink to information to help you address the problem.
    link :: Prelude.Maybe Prelude.Text,
    -- | Anomalies that are related to the problem. Use these Anomalies to learn
    -- more about what\'s happening and to help address the issue.
    relatedAnomalies :: Prelude.Maybe [RecommendationRelatedAnomaly],
    -- | The reason DevOps Guru flagged the anomalous behavior as a problem.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the recommendation.
    name :: Prelude.Maybe Prelude.Text,
    -- | Events that are related to the problem. Use these events to learn more
    -- about what\'s happening and to help address the issue.
    relatedEvents :: Prelude.Maybe [RecommendationRelatedEvent],
    -- | A description of the problem.
    description :: Prelude.Maybe Prelude.Text
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
-- 'link', 'recommendation_link' - A hyperlink to information to help you address the problem.
--
-- 'relatedAnomalies', 'recommendation_relatedAnomalies' - Anomalies that are related to the problem. Use these Anomalies to learn
-- more about what\'s happening and to help address the issue.
--
-- 'reason', 'recommendation_reason' - The reason DevOps Guru flagged the anomalous behavior as a problem.
--
-- 'name', 'recommendation_name' - The name of the recommendation.
--
-- 'relatedEvents', 'recommendation_relatedEvents' - Events that are related to the problem. Use these events to learn more
-- about what\'s happening and to help address the issue.
--
-- 'description', 'recommendation_description' - A description of the problem.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { link = Prelude.Nothing,
      relatedAnomalies = Prelude.Nothing,
      reason = Prelude.Nothing,
      name = Prelude.Nothing,
      relatedEvents = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A hyperlink to information to help you address the problem.
recommendation_link :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_link = Lens.lens (\Recommendation' {link} -> link) (\s@Recommendation' {} a -> s {link = a} :: Recommendation)

-- | Anomalies that are related to the problem. Use these Anomalies to learn
-- more about what\'s happening and to help address the issue.
recommendation_relatedAnomalies :: Lens.Lens' Recommendation (Prelude.Maybe [RecommendationRelatedAnomaly])
recommendation_relatedAnomalies = Lens.lens (\Recommendation' {relatedAnomalies} -> relatedAnomalies) (\s@Recommendation' {} a -> s {relatedAnomalies = a} :: Recommendation) Prelude.. Lens.mapping Lens.coerced

-- | The reason DevOps Guru flagged the anomalous behavior as a problem.
recommendation_reason :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_reason = Lens.lens (\Recommendation' {reason} -> reason) (\s@Recommendation' {} a -> s {reason = a} :: Recommendation)

-- | The name of the recommendation.
recommendation_name :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_name = Lens.lens (\Recommendation' {name} -> name) (\s@Recommendation' {} a -> s {name = a} :: Recommendation)

-- | Events that are related to the problem. Use these events to learn more
-- about what\'s happening and to help address the issue.
recommendation_relatedEvents :: Lens.Lens' Recommendation (Prelude.Maybe [RecommendationRelatedEvent])
recommendation_relatedEvents = Lens.lens (\Recommendation' {relatedEvents} -> relatedEvents) (\s@Recommendation' {} a -> s {relatedEvents = a} :: Recommendation) Prelude.. Lens.mapping Lens.coerced

-- | A description of the problem.
recommendation_description :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_description = Lens.lens (\Recommendation' {description} -> description) (\s@Recommendation' {} a -> s {description = a} :: Recommendation)

instance Core.FromJSON Recommendation where
  parseJSON =
    Core.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Core..:? "Link")
            Prelude.<*> ( x Core..:? "RelatedAnomalies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Reason")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RelatedEvents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable Recommendation

instance Prelude.NFData Recommendation
