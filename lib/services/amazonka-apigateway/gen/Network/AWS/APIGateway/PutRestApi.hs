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
-- Module      : Network.AWS.APIGateway.PutRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for updating an existing
-- API with an input of external API definitions. The update can take the
-- form of merging the supplied definition into the existing API or
-- overwriting the existing API.
module Network.AWS.APIGateway.PutRestApi
  ( -- * Creating a Request
    PutRestApi (..),
    newPutRestApi,

    -- * Request Lenses
    putRestApi_mode,
    putRestApi_failOnWarnings,
    putRestApi_parameters,
    putRestApi_restApiId,
    putRestApi_body,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_policy,
    restApi_disableExecuteApiEndpoint,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A PUT request to update an existing API, with external API definitions
-- specified as the request body.
--
-- /See:/ 'newPutRestApi' smart constructor.
data PutRestApi = PutRestApi'
  { -- | The @mode@ query parameter to specify the update mode. Valid values are
    -- \"merge\" and \"overwrite\". By default, the update mode is \"merge\".
    mode :: Prelude.Maybe PutMode,
    -- | A query parameter to indicate whether to rollback the API update
    -- (@true@) or not (@false@) when a warning is encountered. The default
    -- value is @false@.
    failOnWarnings :: Prelude.Maybe Prelude.Bool,
    -- | Custom header parameters as part of the request. For example, to exclude
    -- DocumentationParts from an imported API, set @ignore=documentation@ as a
    -- @parameters@ value, as in the AWS CLI command of
    -- @aws apigateway import-rest-api --parameters ignore=documentation --body \'file:\/\/\/path\/to\/imported-api-body.json\'@.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The PUT request body containing external API definitions.
    -- Currently, only OpenAPI definition JSON\/YAML files are supported. The
    -- maximum size of the API definition file is 6MB.
    body :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'putRestApi_mode' - The @mode@ query parameter to specify the update mode. Valid values are
-- \"merge\" and \"overwrite\". By default, the update mode is \"merge\".
--
-- 'failOnWarnings', 'putRestApi_failOnWarnings' - A query parameter to indicate whether to rollback the API update
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
--
-- 'parameters', 'putRestApi_parameters' - Custom header parameters as part of the request. For example, to exclude
-- DocumentationParts from an imported API, set @ignore=documentation@ as a
-- @parameters@ value, as in the AWS CLI command of
-- @aws apigateway import-rest-api --parameters ignore=documentation --body \'file:\/\/\/path\/to\/imported-api-body.json\'@.
--
-- 'restApiId', 'putRestApi_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'body', 'putRestApi_body' - [Required] The PUT request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
newPutRestApi ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'body'
  Prelude.ByteString ->
  PutRestApi
newPutRestApi pRestApiId_ pBody_ =
  PutRestApi'
    { mode = Prelude.Nothing,
      failOnWarnings = Prelude.Nothing,
      parameters = Prelude.Nothing,
      restApiId = pRestApiId_,
      body = pBody_
    }

-- | The @mode@ query parameter to specify the update mode. Valid values are
-- \"merge\" and \"overwrite\". By default, the update mode is \"merge\".
putRestApi_mode :: Lens.Lens' PutRestApi (Prelude.Maybe PutMode)
putRestApi_mode = Lens.lens (\PutRestApi' {mode} -> mode) (\s@PutRestApi' {} a -> s {mode = a} :: PutRestApi)

-- | A query parameter to indicate whether to rollback the API update
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
putRestApi_failOnWarnings :: Lens.Lens' PutRestApi (Prelude.Maybe Prelude.Bool)
putRestApi_failOnWarnings = Lens.lens (\PutRestApi' {failOnWarnings} -> failOnWarnings) (\s@PutRestApi' {} a -> s {failOnWarnings = a} :: PutRestApi)

-- | Custom header parameters as part of the request. For example, to exclude
-- DocumentationParts from an imported API, set @ignore=documentation@ as a
-- @parameters@ value, as in the AWS CLI command of
-- @aws apigateway import-rest-api --parameters ignore=documentation --body \'file:\/\/\/path\/to\/imported-api-body.json\'@.
putRestApi_parameters :: Lens.Lens' PutRestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putRestApi_parameters = Lens.lens (\PutRestApi' {parameters} -> parameters) (\s@PutRestApi' {} a -> s {parameters = a} :: PutRestApi) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
putRestApi_restApiId :: Lens.Lens' PutRestApi Prelude.Text
putRestApi_restApiId = Lens.lens (\PutRestApi' {restApiId} -> restApiId) (\s@PutRestApi' {} a -> s {restApiId = a} :: PutRestApi)

-- | [Required] The PUT request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
putRestApi_body :: Lens.Lens' PutRestApi Prelude.ByteString
putRestApi_body = Lens.lens (\PutRestApi' {body} -> body) (\s@PutRestApi' {} a -> s {body = a} :: PutRestApi)

instance Core.AWSRequest PutRestApi where
  type AWSResponse PutRestApi = RestApi
  request = Request.putBody defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable PutRestApi

instance Prelude.NFData PutRestApi

instance Core.ToBody PutRestApi where
  toBody PutRestApi' {..} = Core.toBody body

instance Core.ToHeaders PutRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath PutRestApi where
  toPath PutRestApi' {..} =
    Prelude.mconcat ["/restapis/", Core.toBS restApiId]

instance Core.ToQuery PutRestApi where
  toQuery PutRestApi' {..} =
    Prelude.mconcat
      [ "mode" Core.=: mode,
        "failonwarnings" Core.=: failOnWarnings,
        "parameters"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            )
      ]
