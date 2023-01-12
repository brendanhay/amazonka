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
-- Module      : Amazonka.Glue.Types.UpdateJsonClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UpdateJsonClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a JSON classifier to be updated.
--
-- /See:/ 'newUpdateJsonClassifierRequest' smart constructor.
data UpdateJsonClassifierRequest = UpdateJsonClassifierRequest'
  { -- | A @JsonPath@ string defining the JSON data for the classifier to
    -- classify. Glue supports a subset of JsonPath, as described in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
    jsonPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJsonClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jsonPath', 'updateJsonClassifierRequest_jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
--
-- 'name', 'updateJsonClassifierRequest_name' - The name of the classifier.
newUpdateJsonClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  UpdateJsonClassifierRequest
newUpdateJsonClassifierRequest pName_ =
  UpdateJsonClassifierRequest'
    { jsonPath =
        Prelude.Nothing,
      name = pName_
    }

-- | A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
updateJsonClassifierRequest_jsonPath :: Lens.Lens' UpdateJsonClassifierRequest (Prelude.Maybe Prelude.Text)
updateJsonClassifierRequest_jsonPath = Lens.lens (\UpdateJsonClassifierRequest' {jsonPath} -> jsonPath) (\s@UpdateJsonClassifierRequest' {} a -> s {jsonPath = a} :: UpdateJsonClassifierRequest)

-- | The name of the classifier.
updateJsonClassifierRequest_name :: Lens.Lens' UpdateJsonClassifierRequest Prelude.Text
updateJsonClassifierRequest_name = Lens.lens (\UpdateJsonClassifierRequest' {name} -> name) (\s@UpdateJsonClassifierRequest' {} a -> s {name = a} :: UpdateJsonClassifierRequest)

instance Prelude.Hashable UpdateJsonClassifierRequest where
  hashWithSalt _salt UpdateJsonClassifierRequest' {..} =
    _salt `Prelude.hashWithSalt` jsonPath
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateJsonClassifierRequest where
  rnf UpdateJsonClassifierRequest' {..} =
    Prelude.rnf jsonPath `Prelude.seq` Prelude.rnf name

instance Data.ToJSON UpdateJsonClassifierRequest where
  toJSON UpdateJsonClassifierRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("JsonPath" Data..=) Prelude.<$> jsonPath,
            Prelude.Just ("Name" Data..= name)
          ]
      )
