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
-- Module      : Amazonka.Glue.Types.CreateJsonClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CreateJsonClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a JSON classifier for @CreateClassifier@ to create.
--
-- /See:/ 'newCreateJsonClassifierRequest' smart constructor.
data CreateJsonClassifierRequest = CreateJsonClassifierRequest'
  { -- | The name of the classifier.
    name :: Prelude.Text,
    -- | A @JsonPath@ string defining the JSON data for the classifier to
    -- classify. Glue supports a subset of JsonPath, as described in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
    jsonPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJsonClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createJsonClassifierRequest_name' - The name of the classifier.
--
-- 'jsonPath', 'createJsonClassifierRequest_jsonPath' - A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
newCreateJsonClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  -- | 'jsonPath'
  Prelude.Text ->
  CreateJsonClassifierRequest
newCreateJsonClassifierRequest pName_ pJsonPath_ =
  CreateJsonClassifierRequest'
    { name = pName_,
      jsonPath = pJsonPath_
    }

-- | The name of the classifier.
createJsonClassifierRequest_name :: Lens.Lens' CreateJsonClassifierRequest Prelude.Text
createJsonClassifierRequest_name = Lens.lens (\CreateJsonClassifierRequest' {name} -> name) (\s@CreateJsonClassifierRequest' {} a -> s {name = a} :: CreateJsonClassifierRequest)

-- | A @JsonPath@ string defining the JSON data for the classifier to
-- classify. Glue supports a subset of JsonPath, as described in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers>.
createJsonClassifierRequest_jsonPath :: Lens.Lens' CreateJsonClassifierRequest Prelude.Text
createJsonClassifierRequest_jsonPath = Lens.lens (\CreateJsonClassifierRequest' {jsonPath} -> jsonPath) (\s@CreateJsonClassifierRequest' {} a -> s {jsonPath = a} :: CreateJsonClassifierRequest)

instance Prelude.Hashable CreateJsonClassifierRequest where
  hashWithSalt _salt CreateJsonClassifierRequest' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` jsonPath

instance Prelude.NFData CreateJsonClassifierRequest where
  rnf CreateJsonClassifierRequest' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf jsonPath

instance Data.ToJSON CreateJsonClassifierRequest where
  toJSON CreateJsonClassifierRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("JsonPath" Data..= jsonPath)
          ]
      )
