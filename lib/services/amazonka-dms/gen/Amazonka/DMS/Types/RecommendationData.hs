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
-- Module      : Amazonka.DMS.Types.RecommendationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RecommendationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RdsRecommendation
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the target engine for the specified source
-- database.
--
-- /See:/ 'newRecommendationData' smart constructor.
data RecommendationData = RecommendationData'
  { -- | The recommendation of a target Amazon RDS database engine.
    rdsEngine :: Prelude.Maybe RdsRecommendation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsEngine', 'recommendationData_rdsEngine' - The recommendation of a target Amazon RDS database engine.
newRecommendationData ::
  RecommendationData
newRecommendationData =
  RecommendationData' {rdsEngine = Prelude.Nothing}

-- | The recommendation of a target Amazon RDS database engine.
recommendationData_rdsEngine :: Lens.Lens' RecommendationData (Prelude.Maybe RdsRecommendation)
recommendationData_rdsEngine = Lens.lens (\RecommendationData' {rdsEngine} -> rdsEngine) (\s@RecommendationData' {} a -> s {rdsEngine = a} :: RecommendationData)

instance Data.FromJSON RecommendationData where
  parseJSON =
    Data.withObject
      "RecommendationData"
      ( \x ->
          RecommendationData'
            Prelude.<$> (x Data..:? "RdsEngine")
      )

instance Prelude.Hashable RecommendationData where
  hashWithSalt _salt RecommendationData' {..} =
    _salt `Prelude.hashWithSalt` rdsEngine

instance Prelude.NFData RecommendationData where
  rnf RecommendationData' {..} = Prelude.rnf rdsEngine
