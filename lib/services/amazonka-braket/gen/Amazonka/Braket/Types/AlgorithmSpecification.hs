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
-- Module      : Amazonka.Braket.Types.AlgorithmSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.AlgorithmSpecification where

import Amazonka.Braket.Types.ContainerImage
import Amazonka.Braket.Types.ScriptModeConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines the Amazon Braket job to be created. Specifies the container
-- image the job uses and the paths to the Python scripts used for entry
-- and training.
--
-- /See:/ 'newAlgorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { -- | The container image used to create an Amazon Braket job.
    containerImage :: Prelude.Maybe ContainerImage,
    -- | Configures the paths to the Python scripts used for entry and training.
    scriptModeConfig :: Prelude.Maybe ScriptModeConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerImage', 'algorithmSpecification_containerImage' - The container image used to create an Amazon Braket job.
--
-- 'scriptModeConfig', 'algorithmSpecification_scriptModeConfig' - Configures the paths to the Python scripts used for entry and training.
newAlgorithmSpecification ::
  AlgorithmSpecification
newAlgorithmSpecification =
  AlgorithmSpecification'
    { containerImage =
        Prelude.Nothing,
      scriptModeConfig = Prelude.Nothing
    }

-- | The container image used to create an Amazon Braket job.
algorithmSpecification_containerImage :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe ContainerImage)
algorithmSpecification_containerImage = Lens.lens (\AlgorithmSpecification' {containerImage} -> containerImage) (\s@AlgorithmSpecification' {} a -> s {containerImage = a} :: AlgorithmSpecification)

-- | Configures the paths to the Python scripts used for entry and training.
algorithmSpecification_scriptModeConfig :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe ScriptModeConfig)
algorithmSpecification_scriptModeConfig = Lens.lens (\AlgorithmSpecification' {scriptModeConfig} -> scriptModeConfig) (\s@AlgorithmSpecification' {} a -> s {scriptModeConfig = a} :: AlgorithmSpecification)

instance Core.FromJSON AlgorithmSpecification where
  parseJSON =
    Core.withObject
      "AlgorithmSpecification"
      ( \x ->
          AlgorithmSpecification'
            Prelude.<$> (x Core..:? "containerImage")
            Prelude.<*> (x Core..:? "scriptModeConfig")
      )

instance Prelude.Hashable AlgorithmSpecification where
  hashWithSalt _salt AlgorithmSpecification' {..} =
    _salt `Prelude.hashWithSalt` containerImage
      `Prelude.hashWithSalt` scriptModeConfig

instance Prelude.NFData AlgorithmSpecification where
  rnf AlgorithmSpecification' {..} =
    Prelude.rnf containerImage
      `Prelude.seq` Prelude.rnf scriptModeConfig

instance Core.ToJSON AlgorithmSpecification where
  toJSON AlgorithmSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("containerImage" Core..=)
              Prelude.<$> containerImage,
            ("scriptModeConfig" Core..=)
              Prelude.<$> scriptModeConfig
          ]
      )
