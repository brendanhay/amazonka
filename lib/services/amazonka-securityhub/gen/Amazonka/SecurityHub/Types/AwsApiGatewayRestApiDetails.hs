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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration

-- | Contains information about a REST API in version 1 of Amazon API
-- Gateway.
--
-- /See:/ 'newAwsApiGatewayRestApiDetails' smart constructor.
data AwsApiGatewayRestApiDetails = AwsApiGatewayRestApiDetails'
  { -- | The source of the API key for metering requests according to a usage
    -- plan.
    --
    -- @HEADER@ indicates whether to read the API key from the X-API-Key header
    -- of a request.
    --
    -- @AUTHORIZER@ indicates whether to read the API key from the
    -- @UsageIdentifierKey@ from a custom authorizer.
    apiKeySource :: Prelude.Maybe Prelude.Text,
    -- | The list of binary media types supported by the REST API.
    binaryMediaTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates when the API was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | A description of the REST API.
    description :: Prelude.Maybe Prelude.Text,
    -- | The endpoint configuration of the REST API.
    endpointConfiguration :: Prelude.Maybe AwsApiGatewayEndpointConfiguration,
    -- | The identifier of the REST API.
    id :: Prelude.Maybe Prelude.Text,
    -- | The minimum size in bytes of a payload before compression is enabled.
    --
    -- If @null@, then compression is disabled.
    --
    -- If 0, then all payloads are compressed.
    minimumCompressionSize :: Prelude.Maybe Prelude.Int,
    -- | The name of the REST API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version identifier for the REST API.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayRestApiDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeySource', 'awsApiGatewayRestApiDetails_apiKeySource' - The source of the API key for metering requests according to a usage
-- plan.
--
-- @HEADER@ indicates whether to read the API key from the X-API-Key header
-- of a request.
--
-- @AUTHORIZER@ indicates whether to read the API key from the
-- @UsageIdentifierKey@ from a custom authorizer.
--
-- 'binaryMediaTypes', 'awsApiGatewayRestApiDetails_binaryMediaTypes' - The list of binary media types supported by the REST API.
--
-- 'createdDate', 'awsApiGatewayRestApiDetails_createdDate' - Indicates when the API was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'description', 'awsApiGatewayRestApiDetails_description' - A description of the REST API.
--
-- 'endpointConfiguration', 'awsApiGatewayRestApiDetails_endpointConfiguration' - The endpoint configuration of the REST API.
--
-- 'id', 'awsApiGatewayRestApiDetails_id' - The identifier of the REST API.
--
-- 'minimumCompressionSize', 'awsApiGatewayRestApiDetails_minimumCompressionSize' - The minimum size in bytes of a payload before compression is enabled.
--
-- If @null@, then compression is disabled.
--
-- If 0, then all payloads are compressed.
--
-- 'name', 'awsApiGatewayRestApiDetails_name' - The name of the REST API.
--
-- 'version', 'awsApiGatewayRestApiDetails_version' - The version identifier for the REST API.
newAwsApiGatewayRestApiDetails ::
  AwsApiGatewayRestApiDetails
newAwsApiGatewayRestApiDetails =
  AwsApiGatewayRestApiDetails'
    { apiKeySource =
        Prelude.Nothing,
      binaryMediaTypes = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      id = Prelude.Nothing,
      minimumCompressionSize = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The source of the API key for metering requests according to a usage
-- plan.
--
-- @HEADER@ indicates whether to read the API key from the X-API-Key header
-- of a request.
--
-- @AUTHORIZER@ indicates whether to read the API key from the
-- @UsageIdentifierKey@ from a custom authorizer.
awsApiGatewayRestApiDetails_apiKeySource :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_apiKeySource = Lens.lens (\AwsApiGatewayRestApiDetails' {apiKeySource} -> apiKeySource) (\s@AwsApiGatewayRestApiDetails' {} a -> s {apiKeySource = a} :: AwsApiGatewayRestApiDetails)

-- | The list of binary media types supported by the REST API.
awsApiGatewayRestApiDetails_binaryMediaTypes :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe [Prelude.Text])
awsApiGatewayRestApiDetails_binaryMediaTypes = Lens.lens (\AwsApiGatewayRestApiDetails' {binaryMediaTypes} -> binaryMediaTypes) (\s@AwsApiGatewayRestApiDetails' {} a -> s {binaryMediaTypes = a} :: AwsApiGatewayRestApiDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the API was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayRestApiDetails_createdDate :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_createdDate = Lens.lens (\AwsApiGatewayRestApiDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayRestApiDetails' {} a -> s {createdDate = a} :: AwsApiGatewayRestApiDetails)

-- | A description of the REST API.
awsApiGatewayRestApiDetails_description :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_description = Lens.lens (\AwsApiGatewayRestApiDetails' {description} -> description) (\s@AwsApiGatewayRestApiDetails' {} a -> s {description = a} :: AwsApiGatewayRestApiDetails)

-- | The endpoint configuration of the REST API.
awsApiGatewayRestApiDetails_endpointConfiguration :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe AwsApiGatewayEndpointConfiguration)
awsApiGatewayRestApiDetails_endpointConfiguration = Lens.lens (\AwsApiGatewayRestApiDetails' {endpointConfiguration} -> endpointConfiguration) (\s@AwsApiGatewayRestApiDetails' {} a -> s {endpointConfiguration = a} :: AwsApiGatewayRestApiDetails)

-- | The identifier of the REST API.
awsApiGatewayRestApiDetails_id :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_id = Lens.lens (\AwsApiGatewayRestApiDetails' {id} -> id) (\s@AwsApiGatewayRestApiDetails' {} a -> s {id = a} :: AwsApiGatewayRestApiDetails)

-- | The minimum size in bytes of a payload before compression is enabled.
--
-- If @null@, then compression is disabled.
--
-- If 0, then all payloads are compressed.
awsApiGatewayRestApiDetails_minimumCompressionSize :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Int)
awsApiGatewayRestApiDetails_minimumCompressionSize = Lens.lens (\AwsApiGatewayRestApiDetails' {minimumCompressionSize} -> minimumCompressionSize) (\s@AwsApiGatewayRestApiDetails' {} a -> s {minimumCompressionSize = a} :: AwsApiGatewayRestApiDetails)

-- | The name of the REST API.
awsApiGatewayRestApiDetails_name :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_name = Lens.lens (\AwsApiGatewayRestApiDetails' {name} -> name) (\s@AwsApiGatewayRestApiDetails' {} a -> s {name = a} :: AwsApiGatewayRestApiDetails)

-- | The version identifier for the REST API.
awsApiGatewayRestApiDetails_version :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_version = Lens.lens (\AwsApiGatewayRestApiDetails' {version} -> version) (\s@AwsApiGatewayRestApiDetails' {} a -> s {version = a} :: AwsApiGatewayRestApiDetails)

instance Data.FromJSON AwsApiGatewayRestApiDetails where
  parseJSON =
    Data.withObject
      "AwsApiGatewayRestApiDetails"
      ( \x ->
          AwsApiGatewayRestApiDetails'
            Prelude.<$> (x Data..:? "ApiKeySource")
            Prelude.<*> ( x
                            Data..:? "BinaryMediaTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndpointConfiguration")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "MinimumCompressionSize")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable AwsApiGatewayRestApiDetails where
  hashWithSalt _salt AwsApiGatewayRestApiDetails' {..} =
    _salt
      `Prelude.hashWithSalt` apiKeySource
      `Prelude.hashWithSalt` binaryMediaTypes
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` minimumCompressionSize
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData AwsApiGatewayRestApiDetails where
  rnf AwsApiGatewayRestApiDetails' {..} =
    Prelude.rnf apiKeySource `Prelude.seq`
      Prelude.rnf binaryMediaTypes `Prelude.seq`
        Prelude.rnf createdDate `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf endpointConfiguration `Prelude.seq`
              Prelude.rnf id `Prelude.seq`
                Prelude.rnf minimumCompressionSize `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf version

instance Data.ToJSON AwsApiGatewayRestApiDetails where
  toJSON AwsApiGatewayRestApiDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApiKeySource" Data..=) Prelude.<$> apiKeySource,
            ("BinaryMediaTypes" Data..=)
              Prelude.<$> binaryMediaTypes,
            ("CreatedDate" Data..=) Prelude.<$> createdDate,
            ("Description" Data..=) Prelude.<$> description,
            ("EndpointConfiguration" Data..=)
              Prelude.<$> endpointConfiguration,
            ("Id" Data..=) Prelude.<$> id,
            ("MinimumCompressionSize" Data..=)
              Prelude.<$> minimumCompressionSize,
            ("Name" Data..=) Prelude.<$> name,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
