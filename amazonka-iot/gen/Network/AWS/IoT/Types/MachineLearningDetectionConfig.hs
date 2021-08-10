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
-- Module      : Network.AWS.IoT.Types.MachineLearningDetectionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MachineLearningDetectionConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ConfidenceLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration of an ML Detect Security Profile.
--
-- /See:/ 'newMachineLearningDetectionConfig' smart constructor.
data MachineLearningDetectionConfig = MachineLearningDetectionConfig'
  { -- | The sensitivity of anomalous behavior evaluation. Can be @Low@,
    -- @Medium@, or @High@.
    confidenceLevel :: ConfidenceLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MachineLearningDetectionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidenceLevel', 'machineLearningDetectionConfig_confidenceLevel' - The sensitivity of anomalous behavior evaluation. Can be @Low@,
-- @Medium@, or @High@.
newMachineLearningDetectionConfig ::
  -- | 'confidenceLevel'
  ConfidenceLevel ->
  MachineLearningDetectionConfig
newMachineLearningDetectionConfig pConfidenceLevel_ =
  MachineLearningDetectionConfig'
    { confidenceLevel =
        pConfidenceLevel_
    }

-- | The sensitivity of anomalous behavior evaluation. Can be @Low@,
-- @Medium@, or @High@.
machineLearningDetectionConfig_confidenceLevel :: Lens.Lens' MachineLearningDetectionConfig ConfidenceLevel
machineLearningDetectionConfig_confidenceLevel = Lens.lens (\MachineLearningDetectionConfig' {confidenceLevel} -> confidenceLevel) (\s@MachineLearningDetectionConfig' {} a -> s {confidenceLevel = a} :: MachineLearningDetectionConfig)

instance Core.FromJSON MachineLearningDetectionConfig where
  parseJSON =
    Core.withObject
      "MachineLearningDetectionConfig"
      ( \x ->
          MachineLearningDetectionConfig'
            Prelude.<$> (x Core..: "confidenceLevel")
      )

instance
  Prelude.Hashable
    MachineLearningDetectionConfig

instance
  Prelude.NFData
    MachineLearningDetectionConfig

instance Core.ToJSON MachineLearningDetectionConfig where
  toJSON MachineLearningDetectionConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("confidenceLevel" Core..= confidenceLevel)
          ]
      )
