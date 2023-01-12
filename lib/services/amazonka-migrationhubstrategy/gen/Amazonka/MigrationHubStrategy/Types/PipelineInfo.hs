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
-- Module      : Amazonka.MigrationHubStrategy.Types.PipelineInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.PipelineInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.PipelineType
import qualified Amazonka.Prelude as Prelude

-- | Detailed information of the pipeline.
--
-- /See:/ 'newPipelineInfo' smart constructor.
data PipelineInfo = PipelineInfo'
  { -- | The time when the pipeline info was configured.
    pipelineConfigurationTimeStamp :: Prelude.Maybe Prelude.Text,
    -- | The type of pipeline.
    pipelineType :: Prelude.Maybe PipelineType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineConfigurationTimeStamp', 'pipelineInfo_pipelineConfigurationTimeStamp' - The time when the pipeline info was configured.
--
-- 'pipelineType', 'pipelineInfo_pipelineType' - The type of pipeline.
newPipelineInfo ::
  PipelineInfo
newPipelineInfo =
  PipelineInfo'
    { pipelineConfigurationTimeStamp =
        Prelude.Nothing,
      pipelineType = Prelude.Nothing
    }

-- | The time when the pipeline info was configured.
pipelineInfo_pipelineConfigurationTimeStamp :: Lens.Lens' PipelineInfo (Prelude.Maybe Prelude.Text)
pipelineInfo_pipelineConfigurationTimeStamp = Lens.lens (\PipelineInfo' {pipelineConfigurationTimeStamp} -> pipelineConfigurationTimeStamp) (\s@PipelineInfo' {} a -> s {pipelineConfigurationTimeStamp = a} :: PipelineInfo)

-- | The type of pipeline.
pipelineInfo_pipelineType :: Lens.Lens' PipelineInfo (Prelude.Maybe PipelineType)
pipelineInfo_pipelineType = Lens.lens (\PipelineInfo' {pipelineType} -> pipelineType) (\s@PipelineInfo' {} a -> s {pipelineType = a} :: PipelineInfo)

instance Data.FromJSON PipelineInfo where
  parseJSON =
    Data.withObject
      "PipelineInfo"
      ( \x ->
          PipelineInfo'
            Prelude.<$> (x Data..:? "pipelineConfigurationTimeStamp")
            Prelude.<*> (x Data..:? "pipelineType")
      )

instance Prelude.Hashable PipelineInfo where
  hashWithSalt _salt PipelineInfo' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineConfigurationTimeStamp
      `Prelude.hashWithSalt` pipelineType

instance Prelude.NFData PipelineInfo where
  rnf PipelineInfo' {..} =
    Prelude.rnf pipelineConfigurationTimeStamp
      `Prelude.seq` Prelude.rnf pipelineType
