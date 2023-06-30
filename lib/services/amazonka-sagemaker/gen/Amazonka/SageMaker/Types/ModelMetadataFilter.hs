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
-- Module      : Amazonka.SageMaker.Types.ModelMetadataFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelMetadataFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelMetadataFilterType

-- | Part of the search expression. You can specify the name and value
-- (domain, task, framework, framework version, task, and model).
--
-- /See:/ 'newModelMetadataFilter' smart constructor.
data ModelMetadataFilter = ModelMetadataFilter'
  { -- | The name of the of the model to filter by.
    name :: ModelMetadataFilterType,
    -- | The value to filter the model metadata.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelMetadataFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'modelMetadataFilter_name' - The name of the of the model to filter by.
--
-- 'value', 'modelMetadataFilter_value' - The value to filter the model metadata.
newModelMetadataFilter ::
  -- | 'name'
  ModelMetadataFilterType ->
  -- | 'value'
  Prelude.Text ->
  ModelMetadataFilter
newModelMetadataFilter pName_ pValue_ =
  ModelMetadataFilter'
    { name = pName_,
      value = pValue_
    }

-- | The name of the of the model to filter by.
modelMetadataFilter_name :: Lens.Lens' ModelMetadataFilter ModelMetadataFilterType
modelMetadataFilter_name = Lens.lens (\ModelMetadataFilter' {name} -> name) (\s@ModelMetadataFilter' {} a -> s {name = a} :: ModelMetadataFilter)

-- | The value to filter the model metadata.
modelMetadataFilter_value :: Lens.Lens' ModelMetadataFilter Prelude.Text
modelMetadataFilter_value = Lens.lens (\ModelMetadataFilter' {value} -> value) (\s@ModelMetadataFilter' {} a -> s {value = a} :: ModelMetadataFilter)

instance Prelude.Hashable ModelMetadataFilter where
  hashWithSalt _salt ModelMetadataFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ModelMetadataFilter where
  rnf ModelMetadataFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ModelMetadataFilter where
  toJSON ModelMetadataFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
