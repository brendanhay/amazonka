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
-- Module      : Amazonka.SageMaker.Types.ModelMetadataSearchExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelMetadataSearchExpression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelMetadataFilter

-- | One or more filters that searches for the specified resource or
-- resources in a search. All resource objects that satisfy the
-- expression\'s condition are included in the search results
--
-- /See:/ 'newModelMetadataSearchExpression' smart constructor.
data ModelMetadataSearchExpression = ModelMetadataSearchExpression'
  { -- | A list of filter objects.
    filters :: Prelude.Maybe (Prelude.NonEmpty ModelMetadataFilter)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelMetadataSearchExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'modelMetadataSearchExpression_filters' - A list of filter objects.
newModelMetadataSearchExpression ::
  ModelMetadataSearchExpression
newModelMetadataSearchExpression =
  ModelMetadataSearchExpression'
    { filters =
        Prelude.Nothing
    }

-- | A list of filter objects.
modelMetadataSearchExpression_filters :: Lens.Lens' ModelMetadataSearchExpression (Prelude.Maybe (Prelude.NonEmpty ModelMetadataFilter))
modelMetadataSearchExpression_filters = Lens.lens (\ModelMetadataSearchExpression' {filters} -> filters) (\s@ModelMetadataSearchExpression' {} a -> s {filters = a} :: ModelMetadataSearchExpression) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ModelMetadataSearchExpression
  where
  hashWithSalt _salt ModelMetadataSearchExpression' {..} =
    _salt `Prelude.hashWithSalt` filters

instance Prelude.NFData ModelMetadataSearchExpression where
  rnf ModelMetadataSearchExpression' {..} =
    Prelude.rnf filters

instance Data.ToJSON ModelMetadataSearchExpression where
  toJSON ModelMetadataSearchExpression' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Filters" Data..=) Prelude.<$> filters]
      )
