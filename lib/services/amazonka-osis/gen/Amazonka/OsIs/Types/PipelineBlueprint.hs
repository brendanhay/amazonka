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
-- Module      : Amazonka.OsIs.Types.PipelineBlueprint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.PipelineBlueprint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for information about an OpenSearch Ingestion blueprint.
--
-- /See:/ 'newPipelineBlueprint' smart constructor.
data PipelineBlueprint = PipelineBlueprint'
  { -- | The name of the blueprint.
    blueprintName :: Prelude.Maybe Prelude.Text,
    -- | The YAML configuration of the blueprint.
    pipelineConfigurationBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintName', 'pipelineBlueprint_blueprintName' - The name of the blueprint.
--
-- 'pipelineConfigurationBody', 'pipelineBlueprint_pipelineConfigurationBody' - The YAML configuration of the blueprint.
newPipelineBlueprint ::
  PipelineBlueprint
newPipelineBlueprint =
  PipelineBlueprint'
    { blueprintName = Prelude.Nothing,
      pipelineConfigurationBody = Prelude.Nothing
    }

-- | The name of the blueprint.
pipelineBlueprint_blueprintName :: Lens.Lens' PipelineBlueprint (Prelude.Maybe Prelude.Text)
pipelineBlueprint_blueprintName = Lens.lens (\PipelineBlueprint' {blueprintName} -> blueprintName) (\s@PipelineBlueprint' {} a -> s {blueprintName = a} :: PipelineBlueprint)

-- | The YAML configuration of the blueprint.
pipelineBlueprint_pipelineConfigurationBody :: Lens.Lens' PipelineBlueprint (Prelude.Maybe Prelude.Text)
pipelineBlueprint_pipelineConfigurationBody = Lens.lens (\PipelineBlueprint' {pipelineConfigurationBody} -> pipelineConfigurationBody) (\s@PipelineBlueprint' {} a -> s {pipelineConfigurationBody = a} :: PipelineBlueprint)

instance Data.FromJSON PipelineBlueprint where
  parseJSON =
    Data.withObject
      "PipelineBlueprint"
      ( \x ->
          PipelineBlueprint'
            Prelude.<$> (x Data..:? "BlueprintName")
            Prelude.<*> (x Data..:? "PipelineConfigurationBody")
      )

instance Prelude.Hashable PipelineBlueprint where
  hashWithSalt _salt PipelineBlueprint' {..} =
    _salt
      `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` pipelineConfigurationBody

instance Prelude.NFData PipelineBlueprint where
  rnf PipelineBlueprint' {..} =
    Prelude.rnf blueprintName
      `Prelude.seq` Prelude.rnf pipelineConfigurationBody
