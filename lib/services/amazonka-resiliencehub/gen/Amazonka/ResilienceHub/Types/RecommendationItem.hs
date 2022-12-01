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
-- Module      : Amazonka.ResilienceHub.Types.RecommendationItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.RecommendationItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines a recommendation.
--
-- /See:/ 'newRecommendationItem' smart constructor.
data RecommendationItem = RecommendationItem'
  { -- | The resource identifier.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the recommendation has already been implemented.
    alreadyImplemented :: Prelude.Maybe Prelude.Bool,
    -- | The target account identifier.
    targetAccountId :: Prelude.Maybe Prelude.Text,
    -- | The target region.
    targetRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'recommendationItem_resourceId' - The resource identifier.
--
-- 'alreadyImplemented', 'recommendationItem_alreadyImplemented' - Specifies if the recommendation has already been implemented.
--
-- 'targetAccountId', 'recommendationItem_targetAccountId' - The target account identifier.
--
-- 'targetRegion', 'recommendationItem_targetRegion' - The target region.
newRecommendationItem ::
  RecommendationItem
newRecommendationItem =
  RecommendationItem'
    { resourceId = Prelude.Nothing,
      alreadyImplemented = Prelude.Nothing,
      targetAccountId = Prelude.Nothing,
      targetRegion = Prelude.Nothing
    }

-- | The resource identifier.
recommendationItem_resourceId :: Lens.Lens' RecommendationItem (Prelude.Maybe Prelude.Text)
recommendationItem_resourceId = Lens.lens (\RecommendationItem' {resourceId} -> resourceId) (\s@RecommendationItem' {} a -> s {resourceId = a} :: RecommendationItem)

-- | Specifies if the recommendation has already been implemented.
recommendationItem_alreadyImplemented :: Lens.Lens' RecommendationItem (Prelude.Maybe Prelude.Bool)
recommendationItem_alreadyImplemented = Lens.lens (\RecommendationItem' {alreadyImplemented} -> alreadyImplemented) (\s@RecommendationItem' {} a -> s {alreadyImplemented = a} :: RecommendationItem)

-- | The target account identifier.
recommendationItem_targetAccountId :: Lens.Lens' RecommendationItem (Prelude.Maybe Prelude.Text)
recommendationItem_targetAccountId = Lens.lens (\RecommendationItem' {targetAccountId} -> targetAccountId) (\s@RecommendationItem' {} a -> s {targetAccountId = a} :: RecommendationItem)

-- | The target region.
recommendationItem_targetRegion :: Lens.Lens' RecommendationItem (Prelude.Maybe Prelude.Text)
recommendationItem_targetRegion = Lens.lens (\RecommendationItem' {targetRegion} -> targetRegion) (\s@RecommendationItem' {} a -> s {targetRegion = a} :: RecommendationItem)

instance Core.FromJSON RecommendationItem where
  parseJSON =
    Core.withObject
      "RecommendationItem"
      ( \x ->
          RecommendationItem'
            Prelude.<$> (x Core..:? "resourceId")
            Prelude.<*> (x Core..:? "alreadyImplemented")
            Prelude.<*> (x Core..:? "targetAccountId")
            Prelude.<*> (x Core..:? "targetRegion")
      )

instance Prelude.Hashable RecommendationItem where
  hashWithSalt _salt RecommendationItem' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` alreadyImplemented
      `Prelude.hashWithSalt` targetAccountId
      `Prelude.hashWithSalt` targetRegion

instance Prelude.NFData RecommendationItem where
  rnf RecommendationItem' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf alreadyImplemented
      `Prelude.seq` Prelude.rnf targetAccountId
      `Prelude.seq` Prelude.rnf targetRegion
