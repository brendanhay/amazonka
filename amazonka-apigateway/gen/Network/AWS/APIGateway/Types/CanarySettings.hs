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
-- Module      : Network.AWS.APIGateway.Types.CanarySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CanarySettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration settings of a canary deployment.
--
-- /See:/ 'newCanarySettings' smart constructor.
data CanarySettings = CanarySettings'
  { -- | The ID of the canary deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The percent (0-100) of traffic diverted to a canary deployment.
    percentTraffic :: Prelude.Maybe Prelude.Double,
    -- | A Boolean flag to indicate whether the canary deployment uses the stage
    -- cache or not.
    useStageCache :: Prelude.Maybe Prelude.Bool,
    -- | Stage variables overridden for a canary release deployment, including
    -- new stage variables introduced in the canary. These stage variables are
    -- represented as a string-to-string map between stage variable names and
    -- their values.
    stageVariableOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CanarySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'canarySettings_deploymentId' - The ID of the canary deployment.
--
-- 'percentTraffic', 'canarySettings_percentTraffic' - The percent (0-100) of traffic diverted to a canary deployment.
--
-- 'useStageCache', 'canarySettings_useStageCache' - A Boolean flag to indicate whether the canary deployment uses the stage
-- cache or not.
--
-- 'stageVariableOverrides', 'canarySettings_stageVariableOverrides' - Stage variables overridden for a canary release deployment, including
-- new stage variables introduced in the canary. These stage variables are
-- represented as a string-to-string map between stage variable names and
-- their values.
newCanarySettings ::
  CanarySettings
newCanarySettings =
  CanarySettings'
    { deploymentId = Prelude.Nothing,
      percentTraffic = Prelude.Nothing,
      useStageCache = Prelude.Nothing,
      stageVariableOverrides = Prelude.Nothing
    }

-- | The ID of the canary deployment.
canarySettings_deploymentId :: Lens.Lens' CanarySettings (Prelude.Maybe Prelude.Text)
canarySettings_deploymentId = Lens.lens (\CanarySettings' {deploymentId} -> deploymentId) (\s@CanarySettings' {} a -> s {deploymentId = a} :: CanarySettings)

-- | The percent (0-100) of traffic diverted to a canary deployment.
canarySettings_percentTraffic :: Lens.Lens' CanarySettings (Prelude.Maybe Prelude.Double)
canarySettings_percentTraffic = Lens.lens (\CanarySettings' {percentTraffic} -> percentTraffic) (\s@CanarySettings' {} a -> s {percentTraffic = a} :: CanarySettings)

-- | A Boolean flag to indicate whether the canary deployment uses the stage
-- cache or not.
canarySettings_useStageCache :: Lens.Lens' CanarySettings (Prelude.Maybe Prelude.Bool)
canarySettings_useStageCache = Lens.lens (\CanarySettings' {useStageCache} -> useStageCache) (\s@CanarySettings' {} a -> s {useStageCache = a} :: CanarySettings)

-- | Stage variables overridden for a canary release deployment, including
-- new stage variables introduced in the canary. These stage variables are
-- represented as a string-to-string map between stage variable names and
-- their values.
canarySettings_stageVariableOverrides :: Lens.Lens' CanarySettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
canarySettings_stageVariableOverrides = Lens.lens (\CanarySettings' {stageVariableOverrides} -> stageVariableOverrides) (\s@CanarySettings' {} a -> s {stageVariableOverrides = a} :: CanarySettings) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CanarySettings where
  parseJSON =
    Prelude.withObject
      "CanarySettings"
      ( \x ->
          CanarySettings'
            Prelude.<$> (x Prelude..:? "deploymentId")
            Prelude.<*> (x Prelude..:? "percentTraffic")
            Prelude.<*> (x Prelude..:? "useStageCache")
            Prelude.<*> ( x Prelude..:? "stageVariableOverrides"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CanarySettings

instance Prelude.NFData CanarySettings

instance Prelude.ToJSON CanarySettings where
  toJSON CanarySettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deploymentId" Prelude..=)
              Prelude.<$> deploymentId,
            ("percentTraffic" Prelude..=)
              Prelude.<$> percentTraffic,
            ("useStageCache" Prelude..=)
              Prelude.<$> useStageCache,
            ("stageVariableOverrides" Prelude..=)
              Prelude.<$> stageVariableOverrides
          ]
      )
