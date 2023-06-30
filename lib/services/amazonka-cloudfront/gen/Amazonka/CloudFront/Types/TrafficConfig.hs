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
-- Module      : Amazonka.CloudFront.Types.TrafficConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.TrafficConfig where

import Amazonka.CloudFront.Types.ContinuousDeploymentPolicyType
import Amazonka.CloudFront.Types.ContinuousDeploymentSingleHeaderConfig
import Amazonka.CloudFront.Types.ContinuousDeploymentSingleWeightConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The traffic configuration of your continuous deployment.
--
-- /See:/ 'newTrafficConfig' smart constructor.
data TrafficConfig = TrafficConfig'
  { -- | Determines which HTTP requests are sent to the staging distribution.
    singleHeaderConfig :: Prelude.Maybe ContinuousDeploymentSingleHeaderConfig,
    -- | Contains the percentage of traffic to send to the staging distribution.
    singleWeightConfig :: Prelude.Maybe ContinuousDeploymentSingleWeightConfig,
    -- | The type of traffic configuration.
    type' :: ContinuousDeploymentPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'singleHeaderConfig', 'trafficConfig_singleHeaderConfig' - Determines which HTTP requests are sent to the staging distribution.
--
-- 'singleWeightConfig', 'trafficConfig_singleWeightConfig' - Contains the percentage of traffic to send to the staging distribution.
--
-- 'type'', 'trafficConfig_type' - The type of traffic configuration.
newTrafficConfig ::
  -- | 'type''
  ContinuousDeploymentPolicyType ->
  TrafficConfig
newTrafficConfig pType_ =
  TrafficConfig'
    { singleHeaderConfig =
        Prelude.Nothing,
      singleWeightConfig = Prelude.Nothing,
      type' = pType_
    }

-- | Determines which HTTP requests are sent to the staging distribution.
trafficConfig_singleHeaderConfig :: Lens.Lens' TrafficConfig (Prelude.Maybe ContinuousDeploymentSingleHeaderConfig)
trafficConfig_singleHeaderConfig = Lens.lens (\TrafficConfig' {singleHeaderConfig} -> singleHeaderConfig) (\s@TrafficConfig' {} a -> s {singleHeaderConfig = a} :: TrafficConfig)

-- | Contains the percentage of traffic to send to the staging distribution.
trafficConfig_singleWeightConfig :: Lens.Lens' TrafficConfig (Prelude.Maybe ContinuousDeploymentSingleWeightConfig)
trafficConfig_singleWeightConfig = Lens.lens (\TrafficConfig' {singleWeightConfig} -> singleWeightConfig) (\s@TrafficConfig' {} a -> s {singleWeightConfig = a} :: TrafficConfig)

-- | The type of traffic configuration.
trafficConfig_type :: Lens.Lens' TrafficConfig ContinuousDeploymentPolicyType
trafficConfig_type = Lens.lens (\TrafficConfig' {type'} -> type') (\s@TrafficConfig' {} a -> s {type' = a} :: TrafficConfig)

instance Data.FromXML TrafficConfig where
  parseXML x =
    TrafficConfig'
      Prelude.<$> (x Data..@? "SingleHeaderConfig")
      Prelude.<*> (x Data..@? "SingleWeightConfig")
      Prelude.<*> (x Data..@ "Type")

instance Prelude.Hashable TrafficConfig where
  hashWithSalt _salt TrafficConfig' {..} =
    _salt
      `Prelude.hashWithSalt` singleHeaderConfig
      `Prelude.hashWithSalt` singleWeightConfig
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TrafficConfig where
  rnf TrafficConfig' {..} =
    Prelude.rnf singleHeaderConfig
      `Prelude.seq` Prelude.rnf singleWeightConfig
      `Prelude.seq` Prelude.rnf type'

instance Data.ToXML TrafficConfig where
  toXML TrafficConfig' {..} =
    Prelude.mconcat
      [ "SingleHeaderConfig" Data.@= singleHeaderConfig,
        "SingleWeightConfig" Data.@= singleWeightConfig,
        "Type" Data.@= type'
      ]
