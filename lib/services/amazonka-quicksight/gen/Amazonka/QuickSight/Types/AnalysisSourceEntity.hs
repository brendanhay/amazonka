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
-- Module      : Amazonka.QuickSight.Types.AnalysisSourceEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisSourceEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisSourceTemplate

-- | The source entity of an analysis.
--
-- /See:/ 'newAnalysisSourceEntity' smart constructor.
data AnalysisSourceEntity = AnalysisSourceEntity'
  { -- | The source template for the source entity of the analysis.
    sourceTemplate :: Prelude.Maybe AnalysisSourceTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSourceEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTemplate', 'analysisSourceEntity_sourceTemplate' - The source template for the source entity of the analysis.
newAnalysisSourceEntity ::
  AnalysisSourceEntity
newAnalysisSourceEntity =
  AnalysisSourceEntity'
    { sourceTemplate =
        Prelude.Nothing
    }

-- | The source template for the source entity of the analysis.
analysisSourceEntity_sourceTemplate :: Lens.Lens' AnalysisSourceEntity (Prelude.Maybe AnalysisSourceTemplate)
analysisSourceEntity_sourceTemplate = Lens.lens (\AnalysisSourceEntity' {sourceTemplate} -> sourceTemplate) (\s@AnalysisSourceEntity' {} a -> s {sourceTemplate = a} :: AnalysisSourceEntity)

instance Prelude.Hashable AnalysisSourceEntity where
  hashWithSalt _salt AnalysisSourceEntity' {..} =
    _salt `Prelude.hashWithSalt` sourceTemplate

instance Prelude.NFData AnalysisSourceEntity where
  rnf AnalysisSourceEntity' {..} =
    Prelude.rnf sourceTemplate

instance Data.ToJSON AnalysisSourceEntity where
  toJSON AnalysisSourceEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceTemplate" Data..=)
              Prelude.<$> sourceTemplate
          ]
      )
