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
-- Module      : Amazonka.AmplifyUiBuilder.Types.Form
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.Form where

import Amazonka.AmplifyUiBuilder.Types.FieldConfig
import Amazonka.AmplifyUiBuilder.Types.FormActionType
import Amazonka.AmplifyUiBuilder.Types.FormCTA
import Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig
import Amazonka.AmplifyUiBuilder.Types.FormStyle
import Amazonka.AmplifyUiBuilder.Types.LabelDecorator
import Amazonka.AmplifyUiBuilder.Types.SectionalElement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration settings for a @Form@ user interface (UI)
-- element for an Amplify app. A form is a component you can add to your
-- project by specifying a data source as the default configuration for the
-- form.
--
-- /See:/ 'newForm' smart constructor.
data Form = Form'
  { -- | Stores the call to action configuration for the form.
    cta :: Prelude.Maybe FormCTA,
    -- | Specifies an icon or decoration to display on the form.
    labelDecorator :: Prelude.Maybe LabelDecorator,
    -- | One or more key-value pairs to use when tagging the form.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID of the Amplify app associated with the form.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the form.
    id :: Prelude.Text,
    -- | The name of the form.
    name :: Prelude.Text,
    -- | The operation to perform on the specified form.
    formActionType :: FormActionType,
    -- | Stores the configuration for the form\'s style.
    style :: FormStyle,
    -- | The type of data source to use to create the form.
    dataType :: FormDataTypeConfig,
    -- | Stores the information about the form\'s fields.
    fields :: Prelude.HashMap Prelude.Text FieldConfig,
    -- | Stores the visual helper elements for the form that are not associated
    -- with any data.
    sectionalElements :: Prelude.HashMap Prelude.Text SectionalElement,
    -- | The schema version of the form when it was imported.
    schemaVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Form' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cta', 'form_cta' - Stores the call to action configuration for the form.
--
-- 'labelDecorator', 'form_labelDecorator' - Specifies an icon or decoration to display on the form.
--
-- 'tags', 'form_tags' - One or more key-value pairs to use when tagging the form.
--
-- 'appId', 'form_appId' - The unique ID of the Amplify app associated with the form.
--
-- 'environmentName', 'form_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'form_id' - The unique ID of the form.
--
-- 'name', 'form_name' - The name of the form.
--
-- 'formActionType', 'form_formActionType' - The operation to perform on the specified form.
--
-- 'style', 'form_style' - Stores the configuration for the form\'s style.
--
-- 'dataType', 'form_dataType' - The type of data source to use to create the form.
--
-- 'fields', 'form_fields' - Stores the information about the form\'s fields.
--
-- 'sectionalElements', 'form_sectionalElements' - Stores the visual helper elements for the form that are not associated
-- with any data.
--
-- 'schemaVersion', 'form_schemaVersion' - The schema version of the form when it was imported.
newForm ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'formActionType'
  FormActionType ->
  -- | 'style'
  FormStyle ->
  -- | 'dataType'
  FormDataTypeConfig ->
  -- | 'schemaVersion'
  Prelude.Text ->
  Form
newForm
  pAppId_
  pEnvironmentName_
  pId_
  pName_
  pFormActionType_
  pStyle_
  pDataType_
  pSchemaVersion_ =
    Form'
      { cta = Prelude.Nothing,
        labelDecorator = Prelude.Nothing,
        tags = Prelude.Nothing,
        appId = pAppId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        name = pName_,
        formActionType = pFormActionType_,
        style = pStyle_,
        dataType = pDataType_,
        fields = Prelude.mempty,
        sectionalElements = Prelude.mempty,
        schemaVersion = pSchemaVersion_
      }

-- | Stores the call to action configuration for the form.
form_cta :: Lens.Lens' Form (Prelude.Maybe FormCTA)
form_cta = Lens.lens (\Form' {cta} -> cta) (\s@Form' {} a -> s {cta = a} :: Form)

-- | Specifies an icon or decoration to display on the form.
form_labelDecorator :: Lens.Lens' Form (Prelude.Maybe LabelDecorator)
form_labelDecorator = Lens.lens (\Form' {labelDecorator} -> labelDecorator) (\s@Form' {} a -> s {labelDecorator = a} :: Form)

-- | One or more key-value pairs to use when tagging the form.
form_tags :: Lens.Lens' Form (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
form_tags = Lens.lens (\Form' {tags} -> tags) (\s@Form' {} a -> s {tags = a} :: Form) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the Amplify app associated with the form.
form_appId :: Lens.Lens' Form Prelude.Text
form_appId = Lens.lens (\Form' {appId} -> appId) (\s@Form' {} a -> s {appId = a} :: Form)

-- | The name of the backend environment that is a part of the Amplify app.
form_environmentName :: Lens.Lens' Form Prelude.Text
form_environmentName = Lens.lens (\Form' {environmentName} -> environmentName) (\s@Form' {} a -> s {environmentName = a} :: Form)

-- | The unique ID of the form.
form_id :: Lens.Lens' Form Prelude.Text
form_id = Lens.lens (\Form' {id} -> id) (\s@Form' {} a -> s {id = a} :: Form)

-- | The name of the form.
form_name :: Lens.Lens' Form Prelude.Text
form_name = Lens.lens (\Form' {name} -> name) (\s@Form' {} a -> s {name = a} :: Form)

-- | The operation to perform on the specified form.
form_formActionType :: Lens.Lens' Form FormActionType
form_formActionType = Lens.lens (\Form' {formActionType} -> formActionType) (\s@Form' {} a -> s {formActionType = a} :: Form)

-- | Stores the configuration for the form\'s style.
form_style :: Lens.Lens' Form FormStyle
form_style = Lens.lens (\Form' {style} -> style) (\s@Form' {} a -> s {style = a} :: Form)

-- | The type of data source to use to create the form.
form_dataType :: Lens.Lens' Form FormDataTypeConfig
form_dataType = Lens.lens (\Form' {dataType} -> dataType) (\s@Form' {} a -> s {dataType = a} :: Form)

-- | Stores the information about the form\'s fields.
form_fields :: Lens.Lens' Form (Prelude.HashMap Prelude.Text FieldConfig)
form_fields = Lens.lens (\Form' {fields} -> fields) (\s@Form' {} a -> s {fields = a} :: Form) Prelude.. Lens.coerced

-- | Stores the visual helper elements for the form that are not associated
-- with any data.
form_sectionalElements :: Lens.Lens' Form (Prelude.HashMap Prelude.Text SectionalElement)
form_sectionalElements = Lens.lens (\Form' {sectionalElements} -> sectionalElements) (\s@Form' {} a -> s {sectionalElements = a} :: Form) Prelude.. Lens.coerced

-- | The schema version of the form when it was imported.
form_schemaVersion :: Lens.Lens' Form Prelude.Text
form_schemaVersion = Lens.lens (\Form' {schemaVersion} -> schemaVersion) (\s@Form' {} a -> s {schemaVersion = a} :: Form)

instance Data.FromJSON Form where
  parseJSON =
    Data.withObject
      "Form"
      ( \x ->
          Form'
            Prelude.<$> (x Data..:? "cta")
            Prelude.<*> (x Data..:? "labelDecorator")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "appId")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "formActionType")
            Prelude.<*> (x Data..: "style")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "sectionalElements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "schemaVersion")
      )

instance Prelude.Hashable Form where
  hashWithSalt _salt Form' {..} =
    _salt
      `Prelude.hashWithSalt` cta
      `Prelude.hashWithSalt` labelDecorator
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` formActionType
      `Prelude.hashWithSalt` style
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` sectionalElements
      `Prelude.hashWithSalt` schemaVersion

instance Prelude.NFData Form where
  rnf Form' {..} =
    Prelude.rnf cta
      `Prelude.seq` Prelude.rnf labelDecorator
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf formActionType
      `Prelude.seq` Prelude.rnf style
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf sectionalElements
      `Prelude.seq` Prelude.rnf schemaVersion
