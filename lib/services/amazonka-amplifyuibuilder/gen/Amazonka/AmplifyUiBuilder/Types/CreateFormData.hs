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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CreateFormData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CreateFormData where

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

-- | Represents all of the information that is required to create a form.
--
-- /See:/ 'newCreateFormData' smart constructor.
data CreateFormData = CreateFormData'
  { -- | The @FormCTA@ object that stores the call to action configuration for
    -- the form.
    cta :: Prelude.Maybe FormCTA,
    -- | One or more key-value pairs to use when tagging the form data.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of data source to use to create the form.
    dataType :: FormDataTypeConfig,
    -- | The configuration information for the form\'s fields.
    fields :: Prelude.HashMap Prelude.Text FieldConfig,
    -- | Specifies whether to perform a create or update action on the form.
    formActionType :: FormActionType,
    -- | The name of the form.
    name :: Prelude.Text,
    -- | The schema version of the form.
    schemaVersion :: Prelude.Text,
    -- | The configuration information for the visual helper elements for the
    -- form. These elements are not associated with any data.
    sectionalElements :: Prelude.HashMap Prelude.Text SectionalElement,
    -- | The configuration for the form\'s style.
    style :: FormStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFormData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cta', 'createFormData_cta' - The @FormCTA@ object that stores the call to action configuration for
-- the form.
--
-- 'tags', 'createFormData_tags' - One or more key-value pairs to use when tagging the form data.
--
-- 'dataType', 'createFormData_dataType' - The type of data source to use to create the form.
--
-- 'fields', 'createFormData_fields' - The configuration information for the form\'s fields.
--
-- 'formActionType', 'createFormData_formActionType' - Specifies whether to perform a create or update action on the form.
--
-- 'name', 'createFormData_name' - The name of the form.
--
-- 'schemaVersion', 'createFormData_schemaVersion' - The schema version of the form.
--
-- 'sectionalElements', 'createFormData_sectionalElements' - The configuration information for the visual helper elements for the
-- form. These elements are not associated with any data.
--
-- 'style', 'createFormData_style' - The configuration for the form\'s style.
newCreateFormData ::
  -- | 'dataType'
  FormDataTypeConfig ->
  -- | 'formActionType'
  FormActionType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'schemaVersion'
  Prelude.Text ->
  -- | 'style'
  FormStyle ->
  CreateFormData
newCreateFormData
  pDataType_
  pFormActionType_
  pName_
  pSchemaVersion_
  pStyle_ =
    CreateFormData'
      { cta = Prelude.Nothing,
        tags = Prelude.Nothing,
        dataType = pDataType_,
        fields = Prelude.mempty,
        formActionType = pFormActionType_,
        name = pName_,
        schemaVersion = pSchemaVersion_,
        sectionalElements = Prelude.mempty,
        style = pStyle_
      }

-- | The @FormCTA@ object that stores the call to action configuration for
-- the form.
createFormData_cta :: Lens.Lens' CreateFormData (Prelude.Maybe FormCTA)
createFormData_cta = Lens.lens (\CreateFormData' {cta} -> cta) (\s@CreateFormData' {} a -> s {cta = a} :: CreateFormData)

-- | One or more key-value pairs to use when tagging the form data.
createFormData_tags :: Lens.Lens' CreateFormData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFormData_tags = Lens.lens (\CreateFormData' {tags} -> tags) (\s@CreateFormData' {} a -> s {tags = a} :: CreateFormData) Prelude.. Lens.mapping Lens.coerced

-- | The type of data source to use to create the form.
createFormData_dataType :: Lens.Lens' CreateFormData FormDataTypeConfig
createFormData_dataType = Lens.lens (\CreateFormData' {dataType} -> dataType) (\s@CreateFormData' {} a -> s {dataType = a} :: CreateFormData)

-- | The configuration information for the form\'s fields.
createFormData_fields :: Lens.Lens' CreateFormData (Prelude.HashMap Prelude.Text FieldConfig)
createFormData_fields = Lens.lens (\CreateFormData' {fields} -> fields) (\s@CreateFormData' {} a -> s {fields = a} :: CreateFormData) Prelude.. Lens.coerced

-- | Specifies whether to perform a create or update action on the form.
createFormData_formActionType :: Lens.Lens' CreateFormData FormActionType
createFormData_formActionType = Lens.lens (\CreateFormData' {formActionType} -> formActionType) (\s@CreateFormData' {} a -> s {formActionType = a} :: CreateFormData)

-- | The name of the form.
createFormData_name :: Lens.Lens' CreateFormData Prelude.Text
createFormData_name = Lens.lens (\CreateFormData' {name} -> name) (\s@CreateFormData' {} a -> s {name = a} :: CreateFormData)

-- | The schema version of the form.
createFormData_schemaVersion :: Lens.Lens' CreateFormData Prelude.Text
createFormData_schemaVersion = Lens.lens (\CreateFormData' {schemaVersion} -> schemaVersion) (\s@CreateFormData' {} a -> s {schemaVersion = a} :: CreateFormData)

-- | The configuration information for the visual helper elements for the
-- form. These elements are not associated with any data.
createFormData_sectionalElements :: Lens.Lens' CreateFormData (Prelude.HashMap Prelude.Text SectionalElement)
createFormData_sectionalElements = Lens.lens (\CreateFormData' {sectionalElements} -> sectionalElements) (\s@CreateFormData' {} a -> s {sectionalElements = a} :: CreateFormData) Prelude.. Lens.coerced

-- | The configuration for the form\'s style.
createFormData_style :: Lens.Lens' CreateFormData FormStyle
createFormData_style = Lens.lens (\CreateFormData' {style} -> style) (\s@CreateFormData' {} a -> s {style = a} :: CreateFormData)

instance Prelude.Hashable CreateFormData where
  hashWithSalt _salt CreateFormData' {..} =
    _salt `Prelude.hashWithSalt` cta
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` formActionType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` sectionalElements
      `Prelude.hashWithSalt` style

instance Prelude.NFData CreateFormData where
  rnf CreateFormData' {..} =
    Prelude.rnf cta
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf formActionType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf sectionalElements
      `Prelude.seq` Prelude.rnf style

instance Data.ToJSON CreateFormData where
  toJSON CreateFormData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cta" Data..=) Prelude.<$> cta,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("dataType" Data..= dataType),
            Prelude.Just ("fields" Data..= fields),
            Prelude.Just
              ("formActionType" Data..= formActionType),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("schemaVersion" Data..= schemaVersion),
            Prelude.Just
              ("sectionalElements" Data..= sectionalElements),
            Prelude.Just ("style" Data..= style)
          ]
      )
