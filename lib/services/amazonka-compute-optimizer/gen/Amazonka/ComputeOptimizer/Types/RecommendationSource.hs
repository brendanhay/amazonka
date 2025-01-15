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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationSource where

import Amazonka.ComputeOptimizer.Types.RecommendationSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the source of a recommendation, such as an Amazon EC2 instance
-- or Auto Scaling group.
--
-- /See:/ 'newRecommendationSource' smart constructor.
data RecommendationSource = RecommendationSource'
  { -- | The Amazon Resource Name (ARN) of the recommendation source.
    recommendationSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the recommendation source.
    recommendationSourceType :: Prelude.Maybe RecommendationSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationSourceArn', 'recommendationSource_recommendationSourceArn' - The Amazon Resource Name (ARN) of the recommendation source.
--
-- 'recommendationSourceType', 'recommendationSource_recommendationSourceType' - The resource type of the recommendation source.
newRecommendationSource ::
  RecommendationSource
newRecommendationSource =
  RecommendationSource'
    { recommendationSourceArn =
        Prelude.Nothing,
      recommendationSourceType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the recommendation source.
recommendationSource_recommendationSourceArn :: Lens.Lens' RecommendationSource (Prelude.Maybe Prelude.Text)
recommendationSource_recommendationSourceArn = Lens.lens (\RecommendationSource' {recommendationSourceArn} -> recommendationSourceArn) (\s@RecommendationSource' {} a -> s {recommendationSourceArn = a} :: RecommendationSource)

-- | The resource type of the recommendation source.
recommendationSource_recommendationSourceType :: Lens.Lens' RecommendationSource (Prelude.Maybe RecommendationSourceType)
recommendationSource_recommendationSourceType = Lens.lens (\RecommendationSource' {recommendationSourceType} -> recommendationSourceType) (\s@RecommendationSource' {} a -> s {recommendationSourceType = a} :: RecommendationSource)

instance Data.FromJSON RecommendationSource where
  parseJSON =
    Data.withObject
      "RecommendationSource"
      ( \x ->
          RecommendationSource'
            Prelude.<$> (x Data..:? "recommendationSourceArn")
            Prelude.<*> (x Data..:? "recommendationSourceType")
      )

instance Prelude.Hashable RecommendationSource where
  hashWithSalt _salt RecommendationSource' {..} =
    _salt
      `Prelude.hashWithSalt` recommendationSourceArn
      `Prelude.hashWithSalt` recommendationSourceType

instance Prelude.NFData RecommendationSource where
  rnf RecommendationSource' {..} =
    Prelude.rnf recommendationSourceArn `Prelude.seq`
      Prelude.rnf recommendationSourceType
