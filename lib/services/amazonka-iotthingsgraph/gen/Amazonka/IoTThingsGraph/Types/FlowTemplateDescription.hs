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
-- Module      : Amazonka.IoTThingsGraph.Types.FlowTemplateDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowTemplateDescription where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DefinitionDocument
import Amazonka.IoTThingsGraph.Types.FlowTemplateSummary
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a workflow\'s definition and summary
-- information.
--
-- /See:/ 'newFlowTemplateDescription' smart constructor.
data FlowTemplateDescription = FlowTemplateDescription'
  { -- | The version of the user\'s namespace against which the workflow was
    -- validated. Use this value in your system instance.
    validatedNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | An object that contains summary information about a workflow.
    summary :: Prelude.Maybe FlowTemplateSummary,
    -- | A workflow\'s definition document.
    definition :: Prelude.Maybe DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowTemplateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validatedNamespaceVersion', 'flowTemplateDescription_validatedNamespaceVersion' - The version of the user\'s namespace against which the workflow was
-- validated. Use this value in your system instance.
--
-- 'summary', 'flowTemplateDescription_summary' - An object that contains summary information about a workflow.
--
-- 'definition', 'flowTemplateDescription_definition' - A workflow\'s definition document.
newFlowTemplateDescription ::
  FlowTemplateDescription
newFlowTemplateDescription =
  FlowTemplateDescription'
    { validatedNamespaceVersion =
        Prelude.Nothing,
      summary = Prelude.Nothing,
      definition = Prelude.Nothing
    }

-- | The version of the user\'s namespace against which the workflow was
-- validated. Use this value in your system instance.
flowTemplateDescription_validatedNamespaceVersion :: Lens.Lens' FlowTemplateDescription (Prelude.Maybe Prelude.Integer)
flowTemplateDescription_validatedNamespaceVersion = Lens.lens (\FlowTemplateDescription' {validatedNamespaceVersion} -> validatedNamespaceVersion) (\s@FlowTemplateDescription' {} a -> s {validatedNamespaceVersion = a} :: FlowTemplateDescription)

-- | An object that contains summary information about a workflow.
flowTemplateDescription_summary :: Lens.Lens' FlowTemplateDescription (Prelude.Maybe FlowTemplateSummary)
flowTemplateDescription_summary = Lens.lens (\FlowTemplateDescription' {summary} -> summary) (\s@FlowTemplateDescription' {} a -> s {summary = a} :: FlowTemplateDescription)

-- | A workflow\'s definition document.
flowTemplateDescription_definition :: Lens.Lens' FlowTemplateDescription (Prelude.Maybe DefinitionDocument)
flowTemplateDescription_definition = Lens.lens (\FlowTemplateDescription' {definition} -> definition) (\s@FlowTemplateDescription' {} a -> s {definition = a} :: FlowTemplateDescription)

instance Core.FromJSON FlowTemplateDescription where
  parseJSON =
    Core.withObject
      "FlowTemplateDescription"
      ( \x ->
          FlowTemplateDescription'
            Prelude.<$> (x Core..:? "validatedNamespaceVersion")
            Prelude.<*> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "definition")
      )

instance Prelude.Hashable FlowTemplateDescription where
  hashWithSalt _salt FlowTemplateDescription' {..} =
    _salt
      `Prelude.hashWithSalt` validatedNamespaceVersion
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` definition

instance Prelude.NFData FlowTemplateDescription where
  rnf FlowTemplateDescription' {..} =
    Prelude.rnf validatedNamespaceVersion
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf definition
