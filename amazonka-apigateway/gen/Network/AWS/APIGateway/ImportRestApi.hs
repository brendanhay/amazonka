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
-- Module      : Network.AWS.APIGateway.ImportRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for creating a new API from
-- an external API definition file.
module Network.AWS.APIGateway.ImportRestApi
  ( -- * Creating a Request
    ImportRestApi (..),
    newImportRestApi,

    -- * Request Lenses
    importRestApi_parameters,
    importRestApi_failOnWarnings,
    importRestApi_body,

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
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A POST request to import an API to API Gateway using an input of an API
-- definition file.
--
-- /See:/ 'newImportRestApi' smart constructor.
data ImportRestApi = ImportRestApi'
  { -- | A key-value map of context-specific query string parameters specifying
    -- the behavior of different API importing operations. The following shows
    -- operation-specific parameters and their supported values.
    --
    -- To exclude DocumentationParts from the import, set @parameters@ as
    -- @ignore=documentation@.
    --
    -- To configure the endpoint type, set @parameters@ as
    -- @endpointConfigurationTypes=EDGE@,
    -- @endpointConfigurationTypes=REGIONAL@, or
    -- @endpointConfigurationTypes=PRIVATE@. The default endpoint type is
    -- @EDGE@.
    --
    -- To handle imported @basepath@, set @parameters@ as @basepath=ignore@,
    -- @basepath=prepend@ or @basepath=split@.
    --
    -- For example, the AWS CLI command to exclude documentation from the
    -- imported API is:
    --
    -- > aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'
    --
    -- The AWS CLI command to set the regional endpoint on the imported API is:
    --
    -- > aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A query parameter to indicate whether to rollback the API creation
    -- (@true@) or not (@false@) when a warning is encountered. The default
    -- value is @false@.
    failOnWarnings :: Core.Maybe Core.Bool,
    -- | [Required] The POST request body containing external API definitions.
    -- Currently, only OpenAPI definition JSON\/YAML files are supported. The
    -- maximum size of the API definition file is 6MB.
    body :: Core.ByteString
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'importRestApi_parameters' - A key-value map of context-specific query string parameters specifying
-- the behavior of different API importing operations. The following shows
-- operation-specific parameters and their supported values.
--
-- To exclude DocumentationParts from the import, set @parameters@ as
-- @ignore=documentation@.
--
-- To configure the endpoint type, set @parameters@ as
-- @endpointConfigurationTypes=EDGE@,
-- @endpointConfigurationTypes=REGIONAL@, or
-- @endpointConfigurationTypes=PRIVATE@. The default endpoint type is
-- @EDGE@.
--
-- To handle imported @basepath@, set @parameters@ as @basepath=ignore@,
-- @basepath=prepend@ or @basepath=split@.
--
-- For example, the AWS CLI command to exclude documentation from the
-- imported API is:
--
-- > aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'
--
-- The AWS CLI command to set the regional endpoint on the imported API is:
--
-- > aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'
--
-- 'failOnWarnings', 'importRestApi_failOnWarnings' - A query parameter to indicate whether to rollback the API creation
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
--
-- 'body', 'importRestApi_body' - [Required] The POST request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
newImportRestApi ::
  -- | 'body'
  Core.ByteString ->
  ImportRestApi
newImportRestApi pBody_ =
  ImportRestApi'
    { parameters = Core.Nothing,
      failOnWarnings = Core.Nothing,
      body = pBody_
    }

-- | A key-value map of context-specific query string parameters specifying
-- the behavior of different API importing operations. The following shows
-- operation-specific parameters and their supported values.
--
-- To exclude DocumentationParts from the import, set @parameters@ as
-- @ignore=documentation@.
--
-- To configure the endpoint type, set @parameters@ as
-- @endpointConfigurationTypes=EDGE@,
-- @endpointConfigurationTypes=REGIONAL@, or
-- @endpointConfigurationTypes=PRIVATE@. The default endpoint type is
-- @EDGE@.
--
-- To handle imported @basepath@, set @parameters@ as @basepath=ignore@,
-- @basepath=prepend@ or @basepath=split@.
--
-- For example, the AWS CLI command to exclude documentation from the
-- imported API is:
--
-- > aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'
--
-- The AWS CLI command to set the regional endpoint on the imported API is:
--
-- > aws apigateway import-rest-api --parameters endpointConfigurationTypes=REGIONAL --body 'file:///path/to/imported-api-body.json'
importRestApi_parameters :: Lens.Lens' ImportRestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
importRestApi_parameters = Lens.lens (\ImportRestApi' {parameters} -> parameters) (\s@ImportRestApi' {} a -> s {parameters = a} :: ImportRestApi) Core.. Lens.mapping Lens._Coerce

-- | A query parameter to indicate whether to rollback the API creation
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
importRestApi_failOnWarnings :: Lens.Lens' ImportRestApi (Core.Maybe Core.Bool)
importRestApi_failOnWarnings = Lens.lens (\ImportRestApi' {failOnWarnings} -> failOnWarnings) (\s@ImportRestApi' {} a -> s {failOnWarnings = a} :: ImportRestApi)

-- | [Required] The POST request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
importRestApi_body :: Lens.Lens' ImportRestApi Core.ByteString
importRestApi_body = Lens.lens (\ImportRestApi' {body} -> body) (\s@ImportRestApi' {} a -> s {body = a} :: ImportRestApi)

instance Core.AWSRequest ImportRestApi where
  type AWSResponse ImportRestApi = RestApi
  request = Request.postBody defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable ImportRestApi

instance Core.NFData ImportRestApi

instance Core.ToBody ImportRestApi where
  toBody ImportRestApi' {..} = Core.toBody body

instance Core.ToHeaders ImportRestApi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath ImportRestApi where
  toPath = Core.const "/restapis"

instance Core.ToQuery ImportRestApi where
  toQuery ImportRestApi' {..} =
    Core.mconcat
      [ "parameters"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Core.<$> parameters
            ),
        "failonwarnings" Core.=: failOnWarnings,
        "mode=import"
      ]
