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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormSummary where

import Amazonka.AmplifyUiBuilder.Types.FormActionType
import Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the basic information about a form.
--
-- /See:/ 'newFormSummary' smart constructor.
data FormSummary = FormSummary'
  { -- | The unique ID for the app associated with the form summary.
    appId :: Prelude.Text,
    -- | The form\'s data source type.
    dataType :: FormDataTypeConfig,
    -- | The name of the backend environment that is part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The type of operation to perform on the form.
    formActionType :: FormActionType,
    -- | The ID of the form.
    id :: Prelude.Text,
    -- | The name of the form.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'formSummary_appId' - The unique ID for the app associated with the form summary.
--
-- 'dataType', 'formSummary_dataType' - The form\'s data source type.
--
-- 'environmentName', 'formSummary_environmentName' - The name of the backend environment that is part of the Amplify app.
--
-- 'formActionType', 'formSummary_formActionType' - The type of operation to perform on the form.
--
-- 'id', 'formSummary_id' - The ID of the form.
--
-- 'name', 'formSummary_name' - The name of the form.
newFormSummary ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'dataType'
  FormDataTypeConfig ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'formActionType'
  FormActionType ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  FormSummary
newFormSummary
  pAppId_
  pDataType_
  pEnvironmentName_
  pFormActionType_
  pId_
  pName_ =
    FormSummary'
      { appId = pAppId_,
        dataType = pDataType_,
        environmentName = pEnvironmentName_,
        formActionType = pFormActionType_,
        id = pId_,
        name = pName_
      }

-- | The unique ID for the app associated with the form summary.
formSummary_appId :: Lens.Lens' FormSummary Prelude.Text
formSummary_appId = Lens.lens (\FormSummary' {appId} -> appId) (\s@FormSummary' {} a -> s {appId = a} :: FormSummary)

-- | The form\'s data source type.
formSummary_dataType :: Lens.Lens' FormSummary FormDataTypeConfig
formSummary_dataType = Lens.lens (\FormSummary' {dataType} -> dataType) (\s@FormSummary' {} a -> s {dataType = a} :: FormSummary)

-- | The name of the backend environment that is part of the Amplify app.
formSummary_environmentName :: Lens.Lens' FormSummary Prelude.Text
formSummary_environmentName = Lens.lens (\FormSummary' {environmentName} -> environmentName) (\s@FormSummary' {} a -> s {environmentName = a} :: FormSummary)

-- | The type of operation to perform on the form.
formSummary_formActionType :: Lens.Lens' FormSummary FormActionType
formSummary_formActionType = Lens.lens (\FormSummary' {formActionType} -> formActionType) (\s@FormSummary' {} a -> s {formActionType = a} :: FormSummary)

-- | The ID of the form.
formSummary_id :: Lens.Lens' FormSummary Prelude.Text
formSummary_id = Lens.lens (\FormSummary' {id} -> id) (\s@FormSummary' {} a -> s {id = a} :: FormSummary)

-- | The name of the form.
formSummary_name :: Lens.Lens' FormSummary Prelude.Text
formSummary_name = Lens.lens (\FormSummary' {name} -> name) (\s@FormSummary' {} a -> s {name = a} :: FormSummary)

instance Data.FromJSON FormSummary where
  parseJSON =
    Data.withObject
      "FormSummary"
      ( \x ->
          FormSummary'
            Prelude.<$> (x Data..: "appId")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "environmentName")
            Prelude.<*> (x Data..: "formActionType")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable FormSummary where
  hashWithSalt _salt FormSummary' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` formActionType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData FormSummary where
  rnf FormSummary' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf formActionType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
