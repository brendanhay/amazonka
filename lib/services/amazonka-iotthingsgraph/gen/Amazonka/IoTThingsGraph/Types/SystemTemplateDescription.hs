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
  { -- | The namespace version against which the system was validated. Use this
    -- value in your system instance.
    validatedNamespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | An object that contains summary information about a system.
    summary :: Prelude.Maybe SystemTemplateSummary,
    -- | The definition document of a system.
    definition :: Prelude.Maybe DefinitionDocument
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
-- 'validatedNamespaceVersion', 'systemTemplateDescription_validatedNamespaceVersion' - The namespace version against which the system was validated. Use this
-- value in your system instance.
--
-- 'summary', 'systemTemplateDescription_summary' - An object that contains summary information about a system.
--
-- 'definition', 'systemTemplateDescription_definition' - The definition document of a system.
newSystemTemplateDescription ::
  SystemTemplateDescription
newSystemTemplateDescription =
  SystemTemplateDescription'
    { validatedNamespaceVersion =
        Prelude.Nothing,
      summary = Prelude.Nothing,
      definition = Prelude.Nothing
    }

-- | The namespace version against which the system was validated. Use this
-- value in your system instance.
systemTemplateDescription_validatedNamespaceVersion :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe Prelude.Integer)
systemTemplateDescription_validatedNamespaceVersion = Lens.lens (\SystemTemplateDescription' {validatedNamespaceVersion} -> validatedNamespaceVersion) (\s@SystemTemplateDescription' {} a -> s {validatedNamespaceVersion = a} :: SystemTemplateDescription)

-- | An object that contains summary information about a system.
systemTemplateDescription_summary :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe SystemTemplateSummary)
systemTemplateDescription_summary = Lens.lens (\SystemTemplateDescription' {summary} -> summary) (\s@SystemTemplateDescription' {} a -> s {summary = a} :: SystemTemplateDescription)

-- | The definition document of a system.
systemTemplateDescription_definition :: Lens.Lens' SystemTemplateDescription (Prelude.Maybe DefinitionDocument)
systemTemplateDescription_definition = Lens.lens (\SystemTemplateDescription' {definition} -> definition) (\s@SystemTemplateDescription' {} a -> s {definition = a} :: SystemTemplateDescription)

instance Core.FromJSON SystemTemplateDescription where
  parseJSON =
    Core.withObject
      "SystemTemplateDescription"
      ( \x ->
          SystemTemplateDescription'
            Prelude.<$> (x Core..:? "validatedNamespaceVersion")
            Prelude.<*> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "definition")
      )

instance Prelude.Hashable SystemTemplateDescription where
  hashWithSalt _salt SystemTemplateDescription' {..} =
    _salt
      `Prelude.hashWithSalt` validatedNamespaceVersion
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` definition

instance Prelude.NFData SystemTemplateDescription where
  rnf SystemTemplateDescription' {..} =
    Prelude.rnf validatedNamespaceVersion
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf definition
