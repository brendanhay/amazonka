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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataEnum where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the enums in a generic data schema.
--
-- /See:/ 'newCodegenGenericDataEnum' smart constructor.
data CodegenGenericDataEnum = CodegenGenericDataEnum'
  { -- | The list of enum values in the generic data schema.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenGenericDataEnum' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'codegenGenericDataEnum_values' - The list of enum values in the generic data schema.
newCodegenGenericDataEnum ::
  CodegenGenericDataEnum
newCodegenGenericDataEnum =
  CodegenGenericDataEnum' {values = Prelude.mempty}

-- | The list of enum values in the generic data schema.
codegenGenericDataEnum_values :: Lens.Lens' CodegenGenericDataEnum [Prelude.Text]
codegenGenericDataEnum_values = Lens.lens (\CodegenGenericDataEnum' {values} -> values) (\s@CodegenGenericDataEnum' {} a -> s {values = a} :: CodegenGenericDataEnum) Prelude.. Lens.coerced

instance Data.FromJSON CodegenGenericDataEnum where
  parseJSON =
    Data.withObject
      "CodegenGenericDataEnum"
      ( \x ->
          CodegenGenericDataEnum'
            Prelude.<$> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CodegenGenericDataEnum where
  hashWithSalt _salt CodegenGenericDataEnum' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData CodegenGenericDataEnum where
  rnf CodegenGenericDataEnum' {..} = Prelude.rnf values

instance Data.ToJSON CodegenGenericDataEnum where
  toJSON CodegenGenericDataEnum' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("values" Data..= values)]
      )
