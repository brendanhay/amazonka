{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.GetModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing model defined for a RestApi resource.
module Amazonka.APIGateway.GetModel
  ( -- * Creating a Request
    GetModel (..),
    newGetModel,

    -- * Request Lenses
    getModel_flatten,
    getModel_restApiId,
    getModel_modelName,

    -- * Destructuring the Response
    Model (..),
    newModel,

    -- * Response Lenses
    model_contentType,
    model_description,
    model_id,
    model_name,
    model_schema,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to list information about a model in an existing RestApi
-- resource.
--
-- /See:/ 'newGetModel' smart constructor.
data GetModel = GetModel'
  { -- | A query parameter of a Boolean value to resolve (@true@) all external
    -- model references and returns a flattened model schema or not (@false@)
    -- The default is @false@.
    flatten :: Prelude.Maybe Prelude.Bool,
    -- | The RestApi identifier under which the Model exists.
    restApiId :: Prelude.Text,
    -- | The name of the model as an identifier.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flatten', 'getModel_flatten' - A query parameter of a Boolean value to resolve (@true@) all external
-- model references and returns a flattened model schema or not (@false@)
-- The default is @false@.
--
-- 'restApiId', 'getModel_restApiId' - The RestApi identifier under which the Model exists.
--
-- 'modelName', 'getModel_modelName' - The name of the model as an identifier.
newGetModel ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  GetModel
newGetModel pRestApiId_ pModelName_ =
  GetModel'
    { flatten = Prelude.Nothing,
      restApiId = pRestApiId_,
      modelName = pModelName_
    }

-- | A query parameter of a Boolean value to resolve (@true@) all external
-- model references and returns a flattened model schema or not (@false@)
-- The default is @false@.
getModel_flatten :: Lens.Lens' GetModel (Prelude.Maybe Prelude.Bool)
getModel_flatten = Lens.lens (\GetModel' {flatten} -> flatten) (\s@GetModel' {} a -> s {flatten = a} :: GetModel)

-- | The RestApi identifier under which the Model exists.
getModel_restApiId :: Lens.Lens' GetModel Prelude.Text
getModel_restApiId = Lens.lens (\GetModel' {restApiId} -> restApiId) (\s@GetModel' {} a -> s {restApiId = a} :: GetModel)

-- | The name of the model as an identifier.
getModel_modelName :: Lens.Lens' GetModel Prelude.Text
getModel_modelName = Lens.lens (\GetModel' {modelName} -> modelName) (\s@GetModel' {} a -> s {modelName = a} :: GetModel)

instance Core.AWSRequest GetModel where
  type AWSResponse GetModel = Model
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetModel where
  hashWithSalt _salt GetModel' {..} =
    _salt
      `Prelude.hashWithSalt` flatten
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData GetModel where
  rnf GetModel' {..} =
    Prelude.rnf flatten
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf modelName

instance Data.ToHeaders GetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetModel where
  toPath GetModel' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/models/",
        Data.toBS modelName
      ]

instance Data.ToQuery GetModel where
  toQuery GetModel' {..} =
    Prelude.mconcat ["flatten" Data.=: flatten]
