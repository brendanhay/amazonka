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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Model where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a data model for an API. Supported only for WebSocket APIs.
-- See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html Create Models and Mapping Templates for Request and Response Mappings>.
--
-- /See:/ 'newModel' smart constructor.
data Model = Model'
  { -- | The description of the model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema for the model. For application\/json models, this should be
    -- JSON schema draft 4 model.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The model identifier.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The content-type for the model, for example, \"application\/json\".
    contentType :: Prelude.Maybe Prelude.Text,
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
-- 'description', 'model_description' - The description of the model.
--
-- 'schema', 'model_schema' - The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
--
-- 'modelId', 'model_modelId' - The model identifier.
--
-- 'contentType', 'model_contentType' - The content-type for the model, for example, \"application\/json\".
--
-- 'name', 'model_name' - The name of the model. Must be alphanumeric.
newModel ::
  -- | 'name'
  Prelude.Text ->
  Model
newModel pName_ =
  Model'
    { description = Prelude.Nothing,
      schema = Prelude.Nothing,
      modelId = Prelude.Nothing,
      contentType = Prelude.Nothing,
      name = pName_
    }

-- | The description of the model.
model_description :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_description = Lens.lens (\Model' {description} -> description) (\s@Model' {} a -> s {description = a} :: Model)

-- | The schema for the model. For application\/json models, this should be
-- JSON schema draft 4 model.
model_schema :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_schema = Lens.lens (\Model' {schema} -> schema) (\s@Model' {} a -> s {schema = a} :: Model)

-- | The model identifier.
model_modelId :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelId = Lens.lens (\Model' {modelId} -> modelId) (\s@Model' {} a -> s {modelId = a} :: Model)

-- | The content-type for the model, for example, \"application\/json\".
model_contentType :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_contentType = Lens.lens (\Model' {contentType} -> contentType) (\s@Model' {} a -> s {contentType = a} :: Model)

-- | The name of the model. Must be alphanumeric.
model_name :: Lens.Lens' Model Prelude.Text
model_name = Lens.lens (\Model' {name} -> name) (\s@Model' {} a -> s {name = a} :: Model)

instance Core.FromJSON Model where
  parseJSON =
    Core.withObject
      "Model"
      ( \x ->
          Model'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..:? "schema")
            Prelude.<*> (x Core..:? "modelId")
            Prelude.<*> (x Core..:? "contentType")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Model where
  hashWithSalt _salt Model' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` name

instance Prelude.NFData Model where
  rnf Model' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf name
