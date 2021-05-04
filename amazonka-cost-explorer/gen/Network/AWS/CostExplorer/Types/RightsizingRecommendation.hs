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
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendation where

import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.RightsizingType
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Recommendations to rightsize resources.
--
-- /See:/ 'newRightsizingRecommendation' smart constructor.
data RightsizingRecommendation = RightsizingRecommendation'
  { -- | The account that this recommendation is for.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Details for termination recommendations.
    terminateRecommendationDetail :: Prelude.Maybe TerminateRecommendationDetail,
    -- | Recommendation to either terminate or modify the resource.
    rightsizingType :: Prelude.Maybe RightsizingType,
    -- | Context regarding the current instance.
    currentInstance :: Prelude.Maybe CurrentInstance,
    -- | Details for modification recommendations.
    modifyRecommendationDetail :: Prelude.Maybe ModifyRecommendationDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RightsizingRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'rightsizingRecommendation_accountId' - The account that this recommendation is for.
--
-- 'terminateRecommendationDetail', 'rightsizingRecommendation_terminateRecommendationDetail' - Details for termination recommendations.
--
-- 'rightsizingType', 'rightsizingRecommendation_rightsizingType' - Recommendation to either terminate or modify the resource.
--
-- 'currentInstance', 'rightsizingRecommendation_currentInstance' - Context regarding the current instance.
--
-- 'modifyRecommendationDetail', 'rightsizingRecommendation_modifyRecommendationDetail' - Details for modification recommendations.
newRightsizingRecommendation ::
  RightsizingRecommendation
newRightsizingRecommendation =
  RightsizingRecommendation'
    { accountId =
        Prelude.Nothing,
      terminateRecommendationDetail = Prelude.Nothing,
      rightsizingType = Prelude.Nothing,
      currentInstance = Prelude.Nothing,
      modifyRecommendationDetail = Prelude.Nothing
    }

-- | The account that this recommendation is for.
rightsizingRecommendation_accountId :: Lens.Lens' RightsizingRecommendation (Prelude.Maybe Prelude.Text)
rightsizingRecommendation_accountId = Lens.lens (\RightsizingRecommendation' {accountId} -> accountId) (\s@RightsizingRecommendation' {} a -> s {accountId = a} :: RightsizingRecommendation)

-- | Details for termination recommendations.
rightsizingRecommendation_terminateRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Prelude.Maybe TerminateRecommendationDetail)
rightsizingRecommendation_terminateRecommendationDetail = Lens.lens (\RightsizingRecommendation' {terminateRecommendationDetail} -> terminateRecommendationDetail) (\s@RightsizingRecommendation' {} a -> s {terminateRecommendationDetail = a} :: RightsizingRecommendation)

-- | Recommendation to either terminate or modify the resource.
rightsizingRecommendation_rightsizingType :: Lens.Lens' RightsizingRecommendation (Prelude.Maybe RightsizingType)
rightsizingRecommendation_rightsizingType = Lens.lens (\RightsizingRecommendation' {rightsizingType} -> rightsizingType) (\s@RightsizingRecommendation' {} a -> s {rightsizingType = a} :: RightsizingRecommendation)

-- | Context regarding the current instance.
rightsizingRecommendation_currentInstance :: Lens.Lens' RightsizingRecommendation (Prelude.Maybe CurrentInstance)
rightsizingRecommendation_currentInstance = Lens.lens (\RightsizingRecommendation' {currentInstance} -> currentInstance) (\s@RightsizingRecommendation' {} a -> s {currentInstance = a} :: RightsizingRecommendation)

-- | Details for modification recommendations.
rightsizingRecommendation_modifyRecommendationDetail :: Lens.Lens' RightsizingRecommendation (Prelude.Maybe ModifyRecommendationDetail)
rightsizingRecommendation_modifyRecommendationDetail = Lens.lens (\RightsizingRecommendation' {modifyRecommendationDetail} -> modifyRecommendationDetail) (\s@RightsizingRecommendation' {} a -> s {modifyRecommendationDetail = a} :: RightsizingRecommendation)

instance Prelude.FromJSON RightsizingRecommendation where
  parseJSON =
    Prelude.withObject
      "RightsizingRecommendation"
      ( \x ->
          RightsizingRecommendation'
            Prelude.<$> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "TerminateRecommendationDetail")
            Prelude.<*> (x Prelude..:? "RightsizingType")
            Prelude.<*> (x Prelude..:? "CurrentInstance")
            Prelude.<*> (x Prelude..:? "ModifyRecommendationDetail")
      )

instance Prelude.Hashable RightsizingRecommendation

instance Prelude.NFData RightsizingRecommendation
