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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig where

import Amazonka.AmplifyUiBuilder.Types.FormDataSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the data type configuration for the data source associated
-- with a form.
--
-- /See:/ 'newFormDataTypeConfig' smart constructor.
data FormDataTypeConfig = FormDataTypeConfig'
  { -- | The data source type, either an Amplify DataStore model or a custom data
    -- type.
    dataSourceType :: FormDataSourceType,
    -- | The unique name of the data type you are using as the data source for
    -- the form.
    dataTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FormDataTypeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceType', 'formDataTypeConfig_dataSourceType' - The data source type, either an Amplify DataStore model or a custom data
-- type.
--
-- 'dataTypeName', 'formDataTypeConfig_dataTypeName' - The unique name of the data type you are using as the data source for
-- the form.
newFormDataTypeConfig ::
  -- | 'dataSourceType'
  FormDataSourceType ->
  -- | 'dataTypeName'
  Prelude.Text ->
  FormDataTypeConfig
newFormDataTypeConfig pDataSourceType_ pDataTypeName_ =
  FormDataTypeConfig'
    { dataSourceType =
        pDataSourceType_,
      dataTypeName = pDataTypeName_
    }

-- | The data source type, either an Amplify DataStore model or a custom data
-- type.
formDataTypeConfig_dataSourceType :: Lens.Lens' FormDataTypeConfig FormDataSourceType
formDataTypeConfig_dataSourceType = Lens.lens (\FormDataTypeConfig' {dataSourceType} -> dataSourceType) (\s@FormDataTypeConfig' {} a -> s {dataSourceType = a} :: FormDataTypeConfig)

-- | The unique name of the data type you are using as the data source for
-- the form.
formDataTypeConfig_dataTypeName :: Lens.Lens' FormDataTypeConfig Prelude.Text
formDataTypeConfig_dataTypeName = Lens.lens (\FormDataTypeConfig' {dataTypeName} -> dataTypeName) (\s@FormDataTypeConfig' {} a -> s {dataTypeName = a} :: FormDataTypeConfig)

instance Data.FromJSON FormDataTypeConfig where
  parseJSON =
    Data.withObject
      "FormDataTypeConfig"
      ( \x ->
          FormDataTypeConfig'
            Prelude.<$> (x Data..: "dataSourceType")
            Prelude.<*> (x Data..: "dataTypeName")
      )

instance Prelude.Hashable FormDataTypeConfig where
  hashWithSalt _salt FormDataTypeConfig' {..} =
    _salt `Prelude.hashWithSalt` dataSourceType
      `Prelude.hashWithSalt` dataTypeName

instance Prelude.NFData FormDataTypeConfig where
  rnf FormDataTypeConfig' {..} =
    Prelude.rnf dataSourceType
      `Prelude.seq` Prelude.rnf dataTypeName

instance Data.ToJSON FormDataTypeConfig where
  toJSON FormDataTypeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("dataSourceType" Data..= dataSourceType),
            Prelude.Just ("dataTypeName" Data..= dataTypeName)
          ]
      )
