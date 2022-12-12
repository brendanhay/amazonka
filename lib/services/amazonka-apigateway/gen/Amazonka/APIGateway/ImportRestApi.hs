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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- The AWS CLI command to set the regional endpoint on the imported API is:
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The POST request body containing external API definitions. Currently,
    -- only OpenAPI definition JSON\/YAML files are supported. The maximum size
    -- of the API definition file is 6MB.
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
-- The AWS CLI command to set the regional endpoint on the imported API is:
--
-- 'body', 'importRestApi_body' - The POST request body containing external API definitions. Currently,
-- only OpenAPI definition JSON\/YAML files are supported. The maximum size
-- of the API definition file is 6MB.
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
-- The AWS CLI command to set the regional endpoint on the imported API is:
importRestApi_parameters :: Lens.Lens' ImportRestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importRestApi_parameters = Lens.lens (\ImportRestApi' {parameters} -> parameters) (\s@ImportRestApi' {} a -> s {parameters = a} :: ImportRestApi) Prelude.. Lens.mapping Lens.coerced

-- | The POST request body containing external API definitions. Currently,
-- only OpenAPI definition JSON\/YAML files are supported. The maximum size
-- of the API definition file is 6MB.
importRestApi_body :: Lens.Lens' ImportRestApi Prelude.ByteString
importRestApi_body = Lens.lens (\ImportRestApi' {body} -> body) (\s@ImportRestApi' {} a -> s {body = a} :: ImportRestApi)

instance Core.AWSRequest ImportRestApi where
  type AWSResponse ImportRestApi = RestApi
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ImportRestApi where
  hashWithSalt _salt ImportRestApi' {..} =
    _salt `Prelude.hashWithSalt` failOnWarnings
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` body

instance Prelude.NFData ImportRestApi where
  rnf ImportRestApi' {..} =
    Prelude.rnf failOnWarnings
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf body

instance Data.ToBody ImportRestApi where
  toBody ImportRestApi' {..} = Data.toBody body

instance Data.ToHeaders ImportRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath ImportRestApi where
  toPath = Prelude.const "/restapis"

instance Data.ToQuery ImportRestApi where
  toQuery ImportRestApi' {..} =
    Prelude.mconcat
      [ "failonwarnings" Data.=: failOnWarnings,
        "parameters"
          Data.=: Data.toQuery
            ( Data.toQueryMap "entry" "key" "value"
                Prelude.<$> parameters
            ),
        "mode=import"
      ]
