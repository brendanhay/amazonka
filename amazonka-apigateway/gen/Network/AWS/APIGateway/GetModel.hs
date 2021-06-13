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
-- Module      : Network.AWS.APIGateway.GetModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing model defined for a RestApi resource.
module Network.AWS.APIGateway.GetModel
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
    model_schema,
    model_id,
    model_name,
    model_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list information about a model in an existing RestApi
-- resource.
--
-- /See:/ 'newGetModel' smart constructor.
data GetModel = GetModel'
  { -- | A query parameter of a Boolean value to resolve (@true@) all external
    -- model references and returns a flattened model schema or not (@false@)
    -- The default is @false@.
    flatten :: Prelude.Maybe Prelude.Bool,
    -- | [Required] The RestApi identifier under which the Model exists.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the model as an identifier.
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
-- 'restApiId', 'getModel_restApiId' - [Required] The RestApi identifier under which the Model exists.
--
-- 'modelName', 'getModel_modelName' - [Required] The name of the model as an identifier.
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

-- | [Required] The RestApi identifier under which the Model exists.
getModel_restApiId :: Lens.Lens' GetModel Prelude.Text
getModel_restApiId = Lens.lens (\GetModel' {restApiId} -> restApiId) (\s@GetModel' {} a -> s {restApiId = a} :: GetModel)

-- | [Required] The name of the model as an identifier.
getModel_modelName :: Lens.Lens' GetModel Prelude.Text
getModel_modelName = Lens.lens (\GetModel' {modelName} -> modelName) (\s@GetModel' {} a -> s {modelName = a} :: GetModel)

instance Core.AWSRequest GetModel where
  type AWSResponse GetModel = Model
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetModel

instance Prelude.NFData GetModel

instance Core.ToHeaders GetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetModel where
  toPath GetModel' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/models/",
        Core.toBS modelName
      ]

instance Core.ToQuery GetModel where
  toQuery GetModel' {..} =
    Prelude.mconcat ["flatten" Core.=: flatten]
