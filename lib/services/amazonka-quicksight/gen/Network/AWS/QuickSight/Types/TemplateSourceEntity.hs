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
-- Module      : Network.AWS.QuickSight.Types.TemplateSourceEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.TemplateSourceEntity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.TemplateSourceAnalysis
import Network.AWS.QuickSight.Types.TemplateSourceTemplate

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

instance Prelude.Hashable TemplateSourceEntity

instance Prelude.NFData TemplateSourceEntity

instance Core.ToJSON TemplateSourceEntity where
  toJSON TemplateSourceEntity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceAnalysis" Core..=)
              Prelude.<$> sourceAnalysis,
            ("SourceTemplate" Core..=)
              Prelude.<$> sourceTemplate
          ]
      )
