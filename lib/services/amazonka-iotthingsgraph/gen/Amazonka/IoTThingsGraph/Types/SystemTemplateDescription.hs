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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemTemplateDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemTemplateDescription where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DefinitionDocument
import Amazonka.IoTThingsGraph.Types.SystemTemplateSummary
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains a system\'s definition document and summary
-- information.
--
-- /See:/ 'newSystemTemplateDescription' smart constructor.
data SystemTemplateDescription = SystemTemplateDescription'
  { -- | An object that contains summary information about a system.
    summary :: Prelude.Maybe SystemTemplateSummary,
    -- | The definition document of a system.
    definition :: Prelude.Maybe DefinitionDocument,
    -- | The namespace version against which the system was validated. Use this
    -- value in your system instance.
    validatedNamespaceVersion :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SystemTemplateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'systemTemplateDescription_summary' - An object that contains summary information about a system.
--
-- 'definition', 'systemTemplateDescription_definition' - The definition document of a system.
--
-- 'validatedNamespaceVersion', 'systemTemplateDescription_validatedNamespaceVersion' - The namespace version against which the system was validated. Use this
-- value in your system instance.
newSystemTemplateDescription ::
  SystemTemplateDescription
newSystemTemplateDescription =
  SystemTemplateDescription'
    { summary =
        Prelude.Nothing,
      definition = Prelude.Nothing,
      validatedNamespaceVersion = Prelude.Nothing
    }

-- | An object that contains summary information about a system.
systemTemplateDescription_summary :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe SystemTemplateSummary)
systemTemplateDescription_summary = Lens.lens (\SystemTemplateDescription' {summary} -> summary) (\s@SystemTemplateDescription' {} a -> s {summary = a} :: SystemTemplateDescription)

-- | The definition document of a system.
systemTemplateDescription_definition :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe DefinitionDocument)
systemTemplateDescription_definition = Lens.lens (\SystemTemplateDescription' {definition} -> definition) (\s@SystemTemplateDescription' {} a -> s {definition = a} :: SystemTemplateDescription)

-- | The namespace version against which the system was validated. Use this
-- value in your system instance.
systemTemplateDescription_validatedNamespaceVersion :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe Prelude.Integer)
systemTemplateDescription_validatedNamespaceVersion = Lens.lens (\SystemTemplateDescription' {validatedNamespaceVersion} -> validatedNamespaceVersion) (\s@SystemTemplateDescription' {} a -> s {validatedNamespaceVersion = a} :: SystemTemplateDescription)

instance Core.FromJSON SystemTemplateDescription where
  parseJSON =
    Core.withObject
      "SystemTemplateDescription"
      ( \x ->
          SystemTemplateDescription'
            Prelude.<$> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "definition")
            Prelude.<*> (x Core..:? "validatedNamespaceVersion")
      )

instance Prelude.Hashable SystemTemplateDescription

instance Prelude.NFData SystemTemplateDescription
