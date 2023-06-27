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
-- Module      : Amazonka.DMS.Types.RecommendationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RecommendationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the required target engine settings.
--
-- /See:/ 'newRecommendationSettings' smart constructor.
data RecommendationSettings = RecommendationSettings'
  { -- | The size of your target instance. Fleet Advisor calculates this value
    -- based on your data collection type, such as total capacity and resource
    -- utilization. Valid values include @\"total-capacity\"@ and
    -- @\"utilization\"@.
    instanceSizingType :: Prelude.Text,
    -- | The deployment option for your target engine. For production databases,
    -- Fleet Advisor chooses Multi-AZ deployment. For development or test
    -- databases, Fleet Advisor chooses Single-AZ deployment. Valid values
    -- include @\"development\"@ and @\"production\"@.
    workloadType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSizingType', 'recommendationSettings_instanceSizingType' - The size of your target instance. Fleet Advisor calculates this value
-- based on your data collection type, such as total capacity and resource
-- utilization. Valid values include @\"total-capacity\"@ and
-- @\"utilization\"@.
--
-- 'workloadType', 'recommendationSettings_workloadType' - The deployment option for your target engine. For production databases,
-- Fleet Advisor chooses Multi-AZ deployment. For development or test
-- databases, Fleet Advisor chooses Single-AZ deployment. Valid values
-- include @\"development\"@ and @\"production\"@.
newRecommendationSettings ::
  -- | 'instanceSizingType'
  Prelude.Text ->
  -- | 'workloadType'
  Prelude.Text ->
  RecommendationSettings
newRecommendationSettings
  pInstanceSizingType_
  pWorkloadType_ =
    RecommendationSettings'
      { instanceSizingType =
          pInstanceSizingType_,
        workloadType = pWorkloadType_
      }

-- | The size of your target instance. Fleet Advisor calculates this value
-- based on your data collection type, such as total capacity and resource
-- utilization. Valid values include @\"total-capacity\"@ and
-- @\"utilization\"@.
recommendationSettings_instanceSizingType :: Lens.Lens' RecommendationSettings Prelude.Text
recommendationSettings_instanceSizingType = Lens.lens (\RecommendationSettings' {instanceSizingType} -> instanceSizingType) (\s@RecommendationSettings' {} a -> s {instanceSizingType = a} :: RecommendationSettings)

-- | The deployment option for your target engine. For production databases,
-- Fleet Advisor chooses Multi-AZ deployment. For development or test
-- databases, Fleet Advisor chooses Single-AZ deployment. Valid values
-- include @\"development\"@ and @\"production\"@.
recommendationSettings_workloadType :: Lens.Lens' RecommendationSettings Prelude.Text
recommendationSettings_workloadType = Lens.lens (\RecommendationSettings' {workloadType} -> workloadType) (\s@RecommendationSettings' {} a -> s {workloadType = a} :: RecommendationSettings)

instance Data.FromJSON RecommendationSettings where
  parseJSON =
    Data.withObject
      "RecommendationSettings"
      ( \x ->
          RecommendationSettings'
            Prelude.<$> (x Data..: "InstanceSizingType")
            Prelude.<*> (x Data..: "WorkloadType")
      )

instance Prelude.Hashable RecommendationSettings where
  hashWithSalt _salt RecommendationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` instanceSizingType
      `Prelude.hashWithSalt` workloadType

instance Prelude.NFData RecommendationSettings where
  rnf RecommendationSettings' {..} =
    Prelude.rnf instanceSizingType
      `Prelude.seq` Prelude.rnf workloadType

instance Data.ToJSON RecommendationSettings where
  toJSON RecommendationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("InstanceSizingType" Data..= instanceSizingType),
            Prelude.Just ("WorkloadType" Data..= workloadType)
          ]
      )
