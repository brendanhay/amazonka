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
-- Module      : Amazonka.AmplifyUiBuilder.Types.UpdateFormData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.UpdateFormData where

import Amazonka.AmplifyUiBuilder.Types.FieldConfig
import Amazonka.AmplifyUiBuilder.Types.FormActionType
import Amazonka.AmplifyUiBuilder.Types.FormCTA
import Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig
import Amazonka.AmplifyUiBuilder.Types.FormStyle
import Amazonka.AmplifyUiBuilder.Types.SectionalElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates and saves all of the information about a form, based on form ID.
--
-- /See:/ 'newUpdateFormData' smart constructor.
data UpdateFormData = UpdateFormData'
  { -- | The @FormCTA@ object that stores the call to action configuration for
    -- the form.
    cta :: Prelude.Maybe FormCTA,
    -- | The type of data source to use to create the form.
    dataType :: Prelude.Maybe FormDataTypeConfig,
    -- | The configuration information for the form\'s fields.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text FieldConfig),
    -- | Specifies whether to perform a create or update action on the form.
    formActionType :: Prelude.Maybe FormActionType,
    -- | The name of the form.
    name :: Prelude.Maybe Prelude.Text,
    -- | The schema version of the form.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The configuration information for the visual helper elements for the
    -- form. These elements are not associated with any data.
    sectionalElements :: Prelude.Maybe (Prelude.HashMap Prelude.Text SectionalElement),
    -- | The configuration for the form\'s style.
    style :: Prelude.Maybe FormStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFormData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cta', 'updateFormData_cta' - The @FormCTA@ object that stores the call to action configuration for
-- the form.
--
-- 'dataType', 'updateFormData_dataType' - The type of data source to use to create the form.
--
-- 'fields', 'updateFormData_fields' - The configuration information for the form\'s fields.
--
-- 'formActionType', 'updateFormData_formActionType' - Specifies whether to perform a create or update action on the form.
--
-- 'name', 'updateFormData_name' - The name of the form.
--
-- 'schemaVersion', 'updateFormData_schemaVersion' - The schema version of the form.
--
-- 'sectionalElements', 'updateFormData_sectionalElements' - The configuration information for the visual helper elements for the
-- form. These elements are not associated with any data.
--
-- 'style', 'updateFormData_style' - The configuration for the form\'s style.
newUpdateFormData ::
  UpdateFormData
newUpdateFormData =
  UpdateFormData'
    { cta = Prelude.Nothing,
      dataType = Prelude.Nothing,
      fields = Prelude.Nothing,
      formActionType = Prelude.Nothing,
      name = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      sectionalElements = Prelude.Nothing,
      style = Prelude.Nothing
    }

-- | The @FormCTA@ object that stores the call to action configuration for
-- the form.
updateFormData_cta :: Lens.Lens' UpdateFormData (Prelude.Maybe FormCTA)
updateFormData_cta = Lens.lens (\UpdateFormData' {cta} -> cta) (\s@UpdateFormData' {} a -> s {cta = a} :: UpdateFormData)

-- | The type of data source to use to create the form.
updateFormData_dataType :: Lens.Lens' UpdateFormData (Prelude.Maybe FormDataTypeConfig)
updateFormData_dataType = Lens.lens (\UpdateFormData' {dataType} -> dataType) (\s@UpdateFormData' {} a -> s {dataType = a} :: UpdateFormData)

-- | The configuration information for the form\'s fields.
updateFormData_fields :: Lens.Lens' UpdateFormData (Prelude.Maybe (Prelude.HashMap Prelude.Text FieldConfig))
updateFormData_fields = Lens.lens (\UpdateFormData' {fields} -> fields) (\s@UpdateFormData' {} a -> s {fields = a} :: UpdateFormData) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to perform a create or update action on the form.
updateFormData_formActionType :: Lens.Lens' UpdateFormData (Prelude.Maybe FormActionType)
updateFormData_formActionType = Lens.lens (\UpdateFormData' {formActionType} -> formActionType) (\s@UpdateFormData' {} a -> s {formActionType = a} :: UpdateFormData)

-- | The name of the form.
updateFormData_name :: Lens.Lens' UpdateFormData (Prelude.Maybe Prelude.Text)
updateFormData_name = Lens.lens (\UpdateFormData' {name} -> name) (\s@UpdateFormData' {} a -> s {name = a} :: UpdateFormData)

-- | The schema version of the form.
updateFormData_schemaVersion :: Lens.Lens' UpdateFormData (Prelude.Maybe Prelude.Text)
updateFormData_schemaVersion = Lens.lens (\UpdateFormData' {schemaVersion} -> schemaVersion) (\s@UpdateFormData' {} a -> s {schemaVersion = a} :: UpdateFormData)

-- | The configuration information for the visual helper elements for the
-- form. These elements are not associated with any data.
updateFormData_sectionalElements :: Lens.Lens' UpdateFormData (Prelude.Maybe (Prelude.HashMap Prelude.Text SectionalElement))
updateFormData_sectionalElements = Lens.lens (\UpdateFormData' {sectionalElements} -> sectionalElements) (\s@UpdateFormData' {} a -> s {sectionalElements = a} :: UpdateFormData) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for the form\'s style.
updateFormData_style :: Lens.Lens' UpdateFormData (Prelude.Maybe FormStyle)
updateFormData_style = Lens.lens (\UpdateFormData' {style} -> style) (\s@UpdateFormData' {} a -> s {style = a} :: UpdateFormData)

instance Prelude.Hashable UpdateFormData where
  hashWithSalt _salt UpdateFormData' {..} =
    _salt `Prelude.hashWithSalt` cta
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` formActionType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` sectionalElements
      `Prelude.hashWithSalt` style

instance Prelude.NFData UpdateFormData where
  rnf UpdateFormData' {..} =
    Prelude.rnf cta
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf formActionType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf sectionalElements
      `Prelude.seq` Prelude.rnf style

instance Data.ToJSON UpdateFormData where
  toJSON UpdateFormData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cta" Data..=) Prelude.<$> cta,
            ("dataType" Data..=) Prelude.<$> dataType,
            ("fields" Data..=) Prelude.<$> fields,
            ("formActionType" Data..=)
              Prelude.<$> formActionType,
            ("name" Data..=) Prelude.<$> name,
            ("schemaVersion" Data..=) Prelude.<$> schemaVersion,
            ("sectionalElements" Data..=)
              Prelude.<$> sectionalElements,
            ("style" Data..=) Prelude.<$> style
          ]
      )
