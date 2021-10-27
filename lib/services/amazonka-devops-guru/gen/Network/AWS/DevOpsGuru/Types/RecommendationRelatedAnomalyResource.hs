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
-- Module      : Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomalyResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.RecommendationRelatedAnomalyResource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a resource in which DevOps Guru detected anomalous
-- behavior.
--
-- /See:/ 'newRecommendationRelatedAnomalyResource' smart constructor.
data RecommendationRelatedAnomalyResource = RecommendationRelatedAnomalyResource'
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationRelatedAnomalyResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recommendationRelatedAnomalyResource_name' - The name of the resource.
--
-- 'type'', 'recommendationRelatedAnomalyResource_type' - The type of the resource.
newRecommendationRelatedAnomalyResource ::
  RecommendationRelatedAnomalyResource
newRecommendationRelatedAnomalyResource =
  RecommendationRelatedAnomalyResource'
    { name =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the resource.
recommendationRelatedAnomalyResource_name :: Lens.Lens' RecommendationRelatedAnomalyResource (Prelude.Maybe Prelude.Text)
recommendationRelatedAnomalyResource_name = Lens.lens (\RecommendationRelatedAnomalyResource' {name} -> name) (\s@RecommendationRelatedAnomalyResource' {} a -> s {name = a} :: RecommendationRelatedAnomalyResource)

-- | The type of the resource.
recommendationRelatedAnomalyResource_type :: Lens.Lens' RecommendationRelatedAnomalyResource (Prelude.Maybe Prelude.Text)
recommendationRelatedAnomalyResource_type = Lens.lens (\RecommendationRelatedAnomalyResource' {type'} -> type') (\s@RecommendationRelatedAnomalyResource' {} a -> s {type' = a} :: RecommendationRelatedAnomalyResource)

instance
  Core.FromJSON
    RecommendationRelatedAnomalyResource
  where
  parseJSON =
    Core.withObject
      "RecommendationRelatedAnomalyResource"
      ( \x ->
          RecommendationRelatedAnomalyResource'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "Type")
      )

instance
  Prelude.Hashable
    RecommendationRelatedAnomalyResource

instance
  Prelude.NFData
    RecommendationRelatedAnomalyResource
