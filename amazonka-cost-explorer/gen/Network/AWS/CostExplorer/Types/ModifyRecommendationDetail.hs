{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ModifyRecommendationDetail where

import Network.AWS.CostExplorer.Types.TargetInstance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on the modification recommendation.
--
-- /See:/ 'newModifyRecommendationDetail' smart constructor.
data ModifyRecommendationDetail = ModifyRecommendationDetail'
  { -- | Identifies whether this instance type is the AWS default recommendation.
    targetInstances :: Prelude.Maybe [TargetInstance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyRecommendationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetInstances', 'modifyRecommendationDetail_targetInstances' - Identifies whether this instance type is the AWS default recommendation.
newModifyRecommendationDetail ::
  ModifyRecommendationDetail
newModifyRecommendationDetail =
  ModifyRecommendationDetail'
    { targetInstances =
        Prelude.Nothing
    }

-- | Identifies whether this instance type is the AWS default recommendation.
modifyRecommendationDetail_targetInstances :: Lens.Lens' ModifyRecommendationDetail (Prelude.Maybe [TargetInstance])
modifyRecommendationDetail_targetInstances = Lens.lens (\ModifyRecommendationDetail' {targetInstances} -> targetInstances) (\s@ModifyRecommendationDetail' {} a -> s {targetInstances = a} :: ModifyRecommendationDetail) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ModifyRecommendationDetail where
  parseJSON =
    Prelude.withObject
      "ModifyRecommendationDetail"
      ( \x ->
          ModifyRecommendationDetail'
            Prelude.<$> ( x Prelude..:? "TargetInstances"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ModifyRecommendationDetail

instance Prelude.NFData ModifyRecommendationDetail
