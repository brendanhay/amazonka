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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.TemplateInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.TemplateInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.DataType
import qualified Amazonka.Prelude as Prelude

-- | The input parameters of a template.
--
-- /See:/ 'newTemplateInput' smart constructor.
data TemplateInput = TemplateInput'
  { -- | The data type of the template input.
    dataType :: Prelude.Maybe DataType,
    -- | The name of the template.
    inputName :: Prelude.Maybe Prelude.Text,
    -- | Determine if an input is required from the template.
    required :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataType', 'templateInput_dataType' - The data type of the template input.
--
-- 'inputName', 'templateInput_inputName' - The name of the template.
--
-- 'required', 'templateInput_required' - Determine if an input is required from the template.
newTemplateInput ::
  TemplateInput
newTemplateInput =
  TemplateInput'
    { dataType = Prelude.Nothing,
      inputName = Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | The data type of the template input.
templateInput_dataType :: Lens.Lens' TemplateInput (Prelude.Maybe DataType)
templateInput_dataType = Lens.lens (\TemplateInput' {dataType} -> dataType) (\s@TemplateInput' {} a -> s {dataType = a} :: TemplateInput)

-- | The name of the template.
templateInput_inputName :: Lens.Lens' TemplateInput (Prelude.Maybe Prelude.Text)
templateInput_inputName = Lens.lens (\TemplateInput' {inputName} -> inputName) (\s@TemplateInput' {} a -> s {inputName = a} :: TemplateInput)

-- | Determine if an input is required from the template.
templateInput_required :: Lens.Lens' TemplateInput (Prelude.Maybe Prelude.Bool)
templateInput_required = Lens.lens (\TemplateInput' {required} -> required) (\s@TemplateInput' {} a -> s {required = a} :: TemplateInput)

instance Data.FromJSON TemplateInput where
  parseJSON =
    Data.withObject
      "TemplateInput"
      ( \x ->
          TemplateInput'
            Prelude.<$> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "inputName")
            Prelude.<*> (x Data..:? "required")
      )

instance Prelude.Hashable TemplateInput where
  hashWithSalt _salt TemplateInput' {..} =
    _salt
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` inputName
      `Prelude.hashWithSalt` required

instance Prelude.NFData TemplateInput where
  rnf TemplateInput' {..} =
    Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf required
