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
-- Module      : Amazonka.OsIs.Types.PipelineBlueprintSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.PipelineBlueprintSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of an OpenSearch Ingestion blueprint.
--
-- /See:/ 'newPipelineBlueprintSummary' smart constructor.
data PipelineBlueprintSummary = PipelineBlueprintSummary'
  { -- | The name of the blueprint.
    blueprintName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineBlueprintSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintName', 'pipelineBlueprintSummary_blueprintName' - The name of the blueprint.
newPipelineBlueprintSummary ::
  PipelineBlueprintSummary
newPipelineBlueprintSummary =
  PipelineBlueprintSummary'
    { blueprintName =
        Prelude.Nothing
    }

-- | The name of the blueprint.
pipelineBlueprintSummary_blueprintName :: Lens.Lens' PipelineBlueprintSummary (Prelude.Maybe Prelude.Text)
pipelineBlueprintSummary_blueprintName = Lens.lens (\PipelineBlueprintSummary' {blueprintName} -> blueprintName) (\s@PipelineBlueprintSummary' {} a -> s {blueprintName = a} :: PipelineBlueprintSummary)

instance Data.FromJSON PipelineBlueprintSummary where
  parseJSON =
    Data.withObject
      "PipelineBlueprintSummary"
      ( \x ->
          PipelineBlueprintSummary'
            Prelude.<$> (x Data..:? "BlueprintName")
      )

instance Prelude.Hashable PipelineBlueprintSummary where
  hashWithSalt _salt PipelineBlueprintSummary' {..} =
    _salt `Prelude.hashWithSalt` blueprintName

instance Prelude.NFData PipelineBlueprintSummary where
  rnf PipelineBlueprintSummary' {..} =
    Prelude.rnf blueprintName
