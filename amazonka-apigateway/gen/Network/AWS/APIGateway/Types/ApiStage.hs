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
import qualified Network.AWS.Prelude as Prelude

-- | API stage name of the associated API stage in a usage plan.
--
-- /See:/ 'newApiStage' smart constructor.
data ApiStage = ApiStage'
  { -- | API Id of the associated API stage in a usage plan.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | API stage name of the associated API stage in a usage plan.
    stage :: Prelude.Maybe Prelude.Text,
    -- | Map containing method level throttling information for API stage in a
    -- usage plan.
    throttle :: Prelude.Maybe (Prelude.HashMap Prelude.Text ThrottleSettings)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { apiId = Prelude.Nothing,
      stage = Prelude.Nothing,
      throttle = Prelude.Nothing
    }

-- | API Id of the associated API stage in a usage plan.
apiStage_apiId :: Lens.Lens' ApiStage (Prelude.Maybe Prelude.Text)
apiStage_apiId = Lens.lens (\ApiStage' {apiId} -> apiId) (\s@ApiStage' {} a -> s {apiId = a} :: ApiStage)

-- | API stage name of the associated API stage in a usage plan.
apiStage_stage :: Lens.Lens' ApiStage (Prelude.Maybe Prelude.Text)
apiStage_stage = Lens.lens (\ApiStage' {stage} -> stage) (\s@ApiStage' {} a -> s {stage = a} :: ApiStage)

-- | Map containing method level throttling information for API stage in a
-- usage plan.
apiStage_throttle :: Lens.Lens' ApiStage (Prelude.Maybe (Prelude.HashMap Prelude.Text ThrottleSettings))
apiStage_throttle = Lens.lens (\ApiStage' {throttle} -> throttle) (\s@ApiStage' {} a -> s {throttle = a} :: ApiStage) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ApiStage where
  parseJSON =
    Core.withObject
      "ApiStage"
      ( \x ->
          ApiStage'
            Prelude.<$> (x Core..:? "apiId")
            Prelude.<*> (x Core..:? "stage")
            Prelude.<*> (x Core..:? "throttle" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ApiStage

instance Prelude.NFData ApiStage

instance Core.ToJSON ApiStage where
  toJSON ApiStage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("apiId" Core..=) Prelude.<$> apiId,
            ("stage" Core..=) Prelude.<$> stage,
            ("throttle" Core..=) Prelude.<$> throttle
          ]
      )
