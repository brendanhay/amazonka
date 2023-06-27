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
-- Module      : Amazonka.CostExplorer.Types.ModifyRecommendationDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ModifyRecommendationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.TargetInstance
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for the modification recommendation.
--
-- /See:/ 'newModifyRecommendationDetail' smart constructor.
data ModifyRecommendationDetail = ModifyRecommendationDetail'
  { -- | Determines whether this instance type is the Amazon Web Services default
    -- recommendation.
    targetInstances :: Prelude.Maybe [TargetInstance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetInstances', 'modifyRecommendationDetail_targetInstances' - Determines whether this instance type is the Amazon Web Services default
-- recommendation.
newModifyRecommendationDetail ::
  ModifyRecommendationDetail
newModifyRecommendationDetail =
  ModifyRecommendationDetail'
    { targetInstances =
        Prelude.Nothing
    }

-- | Determines whether this instance type is the Amazon Web Services default
-- recommendation.
modifyRecommendationDetail_targetInstances :: Lens.Lens' ModifyRecommendationDetail (Prelude.Maybe [TargetInstance])
modifyRecommendationDetail_targetInstances = Lens.lens (\ModifyRecommendationDetail' {targetInstances} -> targetInstances) (\s@ModifyRecommendationDetail' {} a -> s {targetInstances = a} :: ModifyRecommendationDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ModifyRecommendationDetail where
  parseJSON =
    Data.withObject
      "ModifyRecommendationDetail"
      ( \x ->
          ModifyRecommendationDetail'
            Prelude.<$> ( x
                            Data..:? "TargetInstances"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ModifyRecommendationDetail where
  hashWithSalt _salt ModifyRecommendationDetail' {..} =
    _salt `Prelude.hashWithSalt` targetInstances

instance Prelude.NFData ModifyRecommendationDetail where
  rnf ModifyRecommendationDetail' {..} =
    Prelude.rnf targetInstances
