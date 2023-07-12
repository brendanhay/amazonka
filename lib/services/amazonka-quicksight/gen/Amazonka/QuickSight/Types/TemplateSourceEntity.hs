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
-- Module      : Amazonka.QuickSight.Types.TemplateSourceEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateSourceEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TemplateSourceAnalysis
import Amazonka.QuickSight.Types.TemplateSourceTemplate

-- | The source entity of the template.
--
-- /See:/ 'newTemplateSourceEntity' smart constructor.
data TemplateSourceEntity = TemplateSourceEntity'
  { -- | The source analysis, if it is based on an analysis.
    sourceAnalysis :: Prelude.Maybe TemplateSourceAnalysis,
    -- | The source template, if it is based on an template.
    sourceTemplate :: Prelude.Maybe TemplateSourceTemplate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSourceEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceAnalysis', 'templateSourceEntity_sourceAnalysis' - The source analysis, if it is based on an analysis.
--
-- 'sourceTemplate', 'templateSourceEntity_sourceTemplate' - The source template, if it is based on an template.
newTemplateSourceEntity ::
  TemplateSourceEntity
newTemplateSourceEntity =
  TemplateSourceEntity'
    { sourceAnalysis =
        Prelude.Nothing,
      sourceTemplate = Prelude.Nothing
    }

-- | The source analysis, if it is based on an analysis.
templateSourceEntity_sourceAnalysis :: Lens.Lens' TemplateSourceEntity (Prelude.Maybe TemplateSourceAnalysis)
templateSourceEntity_sourceAnalysis = Lens.lens (\TemplateSourceEntity' {sourceAnalysis} -> sourceAnalysis) (\s@TemplateSourceEntity' {} a -> s {sourceAnalysis = a} :: TemplateSourceEntity)

-- | The source template, if it is based on an template.
templateSourceEntity_sourceTemplate :: Lens.Lens' TemplateSourceEntity (Prelude.Maybe TemplateSourceTemplate)
templateSourceEntity_sourceTemplate = Lens.lens (\TemplateSourceEntity' {sourceTemplate} -> sourceTemplate) (\s@TemplateSourceEntity' {} a -> s {sourceTemplate = a} :: TemplateSourceEntity)

instance Prelude.Hashable TemplateSourceEntity where
  hashWithSalt _salt TemplateSourceEntity' {..} =
    _salt
      `Prelude.hashWithSalt` sourceAnalysis
      `Prelude.hashWithSalt` sourceTemplate

instance Prelude.NFData TemplateSourceEntity where
  rnf TemplateSourceEntity' {..} =
    Prelude.rnf sourceAnalysis
      `Prelude.seq` Prelude.rnf sourceTemplate

instance Data.ToJSON TemplateSourceEntity where
  toJSON TemplateSourceEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceAnalysis" Data..=)
              Prelude.<$> sourceAnalysis,
            ("SourceTemplate" Data..=)
              Prelude.<$> sourceTemplate
          ]
      )
