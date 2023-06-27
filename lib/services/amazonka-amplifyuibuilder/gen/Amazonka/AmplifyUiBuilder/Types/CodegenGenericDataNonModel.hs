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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataNonModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataNonModel where

import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataField
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a non-model in a generic data schema.
--
-- /See:/ 'newCodegenGenericDataNonModel' smart constructor.
data CodegenGenericDataNonModel = CodegenGenericDataNonModel'
  { -- | The fields in a generic data schema non model.
    fields :: Prelude.HashMap Prelude.Text CodegenGenericDataField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenGenericDataNonModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fields', 'codegenGenericDataNonModel_fields' - The fields in a generic data schema non model.
newCodegenGenericDataNonModel ::
  CodegenGenericDataNonModel
newCodegenGenericDataNonModel =
  CodegenGenericDataNonModel'
    { fields =
        Prelude.mempty
    }

-- | The fields in a generic data schema non model.
codegenGenericDataNonModel_fields :: Lens.Lens' CodegenGenericDataNonModel (Prelude.HashMap Prelude.Text CodegenGenericDataField)
codegenGenericDataNonModel_fields = Lens.lens (\CodegenGenericDataNonModel' {fields} -> fields) (\s@CodegenGenericDataNonModel' {} a -> s {fields = a} :: CodegenGenericDataNonModel) Prelude.. Lens.coerced

instance Data.FromJSON CodegenGenericDataNonModel where
  parseJSON =
    Data.withObject
      "CodegenGenericDataNonModel"
      ( \x ->
          CodegenGenericDataNonModel'
            Prelude.<$> (x Data..:? "fields" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CodegenGenericDataNonModel where
  hashWithSalt _salt CodegenGenericDataNonModel' {..} =
    _salt `Prelude.hashWithSalt` fields

instance Prelude.NFData CodegenGenericDataNonModel where
  rnf CodegenGenericDataNonModel' {..} =
    Prelude.rnf fields

instance Data.ToJSON CodegenGenericDataNonModel where
  toJSON CodegenGenericDataNonModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fields" Data..= fields)]
      )
