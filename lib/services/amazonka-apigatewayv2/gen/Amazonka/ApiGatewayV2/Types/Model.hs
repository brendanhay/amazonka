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
-- Module      : Amazonka.ApiGatewayV2.Types.Model
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Model where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a data model for an API. Supported only for WebSocket APIs.
-- See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Create Models and Mapping Templates for Request and Response Mappings>.
--
-- /See:/ 'newModel' smart constructor.
data Model = Model'
  { -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The model identifier.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The name of the model. Must be alphanumeric.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Model' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'model_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'description', 'model_description' - The description of the model.
--
-- 'modelId', 'model_modelId' - The model identifier.
--
-- 'schema', 'model_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'name', 'model_name' - The name of the model. Must be alphanumeric.
newModel ::
  -- | 'name'
  Prelude.Text ->
  Model
newModel pName_ =
  Model'
    { contentType = Prelude.Nothing,
      description = Prelude.Nothing,
      modelId = Prelude.Nothing,
      schema = Prelude.Nothing,
      name = pName_
    }

-- | The content-type for the model, for example, \"application\/json\".
model_contentType :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_contentType = Lens.lens (\Model' {contentType} -> contentType) (\s@Model' {} a -> s {contentType = a} :: Model)

-- | The description of the model.
model_description :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_description = Lens.lens (\Model' {description} -> description) (\s@Model' {} a -> s {description = a} :: Model)

-- | The model identifier.
model_modelId :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelId = Lens.lens (\Model' {modelId} -> modelId) (\s@Model' {} a -> s {modelId = a} :: Model)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
model_schema :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_schema = Lens.lens (\Model' {schema} -> schema) (\s@Model' {} a -> s {schema = a} :: Model)

-- | The name of the model. Must be alphanumeric.
model_name :: Lens.Lens' Model Prelude.Text
model_name = Lens.lens (\Model' {name} -> name) (\s@Model' {} a -> s {name = a} :: Model)

instance Data.FromJSON Model where
  parseJSON =
    Data.withObject
      "Model"
      ( \x ->
          Model'
            Prelude.<$> (x Data..:? "contentType")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "modelId")
            Prelude.<*> (x Data..:? "schema")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Model where
  hashWithSalt _salt Model' {..} =
    _salt `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` name

instance Prelude.NFData Model where
  rnf Model' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf name
