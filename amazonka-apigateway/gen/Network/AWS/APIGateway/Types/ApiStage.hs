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
-- Module      : Network.AWS.APIGateway.Types.ApiStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ApiStage where

import Network.AWS.APIGateway.Types.ThrottleSettings
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | API stage name of the associated API stage in a usage plan.
--
-- /See:/ 'newApiStage' smart constructor.
data ApiStage = ApiStage'
  { -- | API Id of the associated API stage in a usage plan.
    apiId :: Core.Maybe Core.Text,
    -- | API stage name of the associated API stage in a usage plan.
    stage :: Core.Maybe Core.Text,
    -- | Map containing method level throttling information for API stage in a
    -- usage plan.
    throttle :: Core.Maybe (Core.HashMap Core.Text ThrottleSettings)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApiStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'apiStage_apiId' - API Id of the associated API stage in a usage plan.
--
-- 'stage', 'apiStage_stage' - API stage name of the associated API stage in a usage plan.
--
-- 'throttle', 'apiStage_throttle' - Map containing method level throttling information for API stage in a
-- usage plan.
newApiStage ::
  ApiStage
newApiStage =
  ApiStage'
    { apiId = Core.Nothing,
      stage = Core.Nothing,
      throttle = Core.Nothing
    }

-- | API Id of the associated API stage in a usage plan.
apiStage_apiId :: Lens.Lens' ApiStage (Core.Maybe Core.Text)
apiStage_apiId = Lens.lens (\ApiStage' {apiId} -> apiId) (\s@ApiStage' {} a -> s {apiId = a} :: ApiStage)

-- | API stage name of the associated API stage in a usage plan.
apiStage_stage :: Lens.Lens' ApiStage (Core.Maybe Core.Text)
apiStage_stage = Lens.lens (\ApiStage' {stage} -> stage) (\s@ApiStage' {} a -> s {stage = a} :: ApiStage)

-- | Map containing method level throttling information for API stage in a
-- usage plan.
apiStage_throttle :: Lens.Lens' ApiStage (Core.Maybe (Core.HashMap Core.Text ThrottleSettings))
apiStage_throttle = Lens.lens (\ApiStage' {throttle} -> throttle) (\s@ApiStage' {} a -> s {throttle = a} :: ApiStage) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ApiStage where
  parseJSON =
    Core.withObject
      "ApiStage"
      ( \x ->
          ApiStage'
            Core.<$> (x Core..:? "apiId")
            Core.<*> (x Core..:? "stage")
            Core.<*> (x Core..:? "throttle" Core..!= Core.mempty)
      )

instance Core.Hashable ApiStage

instance Core.NFData ApiStage

instance Core.ToJSON ApiStage where
  toJSON ApiStage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("apiId" Core..=) Core.<$> apiId,
            ("stage" Core..=) Core.<$> stage,
            ("throttle" Core..=) Core.<$> throttle
          ]
      )
