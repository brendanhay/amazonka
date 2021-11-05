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
-- Module      : Amazonka.APIGateway.ImportRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for creating a new API from
-- an external API definition file.
module Amazonka.APIGateway.ImportRestApi
  ( -- * Creating a Request
    ImportRestApi (..),
    newImportRestApi,

    -- * Request Lenses
    importRestApi_failOnWarnings,
    importRestApi_parameters,
    importRestApi_body,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_minimumCompressionSize,
    restApi_disableExecuteApiEndpoint,
    restApi_binaryMediaTypes,
    restApi_warnings,
    restApi_createdDate,
    restApi_name,
    restApi_version,
    restApi_apiKeySource,
    restApi_id,
    restApi_policy,
    restApi_endpointConfiguration,
    restApi_description,
    restApi_tags,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A POST request to import an API to API Gateway using an input of an API
-- definition file.
--
-- /See:/ 'newImportRestApi' smart constructor.
data ImportRestApi = ImportRestApi'
  { -- | A query parameter to indicate whether to rollback the API creation
    -- (@true@) or not (@false@) when a warning is encountered. The default
    -- value is @false@.
    failOnWarnings :: Prelude.Maybe Prelude.Bool,
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
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] The POST request body containing external API definitions.
    -- Currently, only OpenAPI definition JSON\/YAML files are supported. The
    -- maximum size of the API definition file is 6MB.
    body :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failOnWarnings', 'importRestApi_failOnWarnings' - A query parameter to indicate whether to rollback the API creation
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
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
-- 'body', 'importRestApi_body' - [Required] The POST request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
newImportRestApi ::
  -- | 'body'
  Prelude.ByteString ->
  ImportRestApi
newImportRestApi pBody_ =
  ImportRestApi'
    { failOnWarnings = Prelude.Nothing,
      parameters = Prelude.Nothing,
      body = pBody_
    }

-- | A query parameter to indicate whether to rollback the API creation
-- (@true@) or not (@false@) when a warning is encountered. The default
-- value is @false@.
importRestApi_failOnWarnings :: Lens.Lens' ImportRestApi (Prelude.Maybe Prelude.Bool)
importRestApi_failOnWarnings = Lens.lens (\ImportRestApi' {failOnWarnings} -> failOnWarnings) (\s@ImportRestApi' {} a -> s {failOnWarnings = a} :: ImportRestApi)

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
importRestApi_parameters :: Lens.Lens' ImportRestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importRestApi_parameters = Lens.lens (\ImportRestApi' {parameters} -> parameters) (\s@ImportRestApi' {} a -> s {parameters = a} :: ImportRestApi) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The POST request body containing external API definitions.
-- Currently, only OpenAPI definition JSON\/YAML files are supported. The
-- maximum size of the API definition file is 6MB.
importRestApi_body :: Lens.Lens' ImportRestApi Prelude.ByteString
importRestApi_body = Lens.lens (\ImportRestApi' {body} -> body) (\s@ImportRestApi' {} a -> s {body = a} :: ImportRestApi)

instance Core.AWSRequest ImportRestApi where
  type AWSResponse ImportRestApi = RestApi
  request = Request.postBody defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable ImportRestApi

instance Prelude.NFData ImportRestApi

instance Core.ToBody ImportRestApi where
  toBody ImportRestApi' {..} = Core.toBody body

instance Core.ToHeaders ImportRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath ImportRestApi where
  toPath = Prelude.const "/restapis"

instance Core.ToQuery ImportRestApi where
  toQuery ImportRestApi' {..} =
    Prelude.mconcat
      [ "failonwarnings" Core.=: failOnWarnings,
        "parameters"
          Core.=: Core.toQuery
            ( Core.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            ),
        "mode=import"
      ]
