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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayRestApiDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration

-- | Contains information about a REST API in version 1 of Amazon API
-- Gateway.
--
-- /See:/ 'newAwsApiGatewayRestApiDetails' smart constructor.
data AwsApiGatewayRestApiDetails = AwsApiGatewayRestApiDetails'
  { -- | The minimum size in bytes of a payload before compression is enabled.
    --
    -- If @null@, then compression is disabled.
    --
    -- If 0, then all payloads are compressed.
    minimumCompressionSize :: Prelude.Maybe Prelude.Int,
    -- | The list of binary media types supported by the REST API.
    binaryMediaTypes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates when the API was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The name of the REST API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source of the API key for metering requests according to a usage
    -- plan.
    --
    -- @HEADER@ indicates whether to read the API key from the X-API-Key header
    -- of a request.
    --
    -- @AUTHORIZER@ indicates whether to read the API key from the
    -- @UsageIdentifierKey@ from a custom authorizer.
    apiKeySource :: Prelude.Maybe Prelude.Text,
    -- | The version identifier for the REST API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the REST API.
    id :: Prelude.Maybe Prelude.Text,
    -- | The endpoint configuration of the REST API.
    endpointConfiguration :: Prelude.Maybe AwsApiGatewayEndpointConfiguration,
    -- | A description of the REST API.
    description :: Prelude.Maybe Prelude.Text
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
-- 'minimumCompressionSize', 'awsApiGatewayRestApiDetails_minimumCompressionSize' - The minimum size in bytes of a payload before compression is enabled.
--
-- If @null@, then compression is disabled.
--
-- If 0, then all payloads are compressed.
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
-- 'name', 'awsApiGatewayRestApiDetails_name' - The name of the REST API.
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
-- 'version', 'awsApiGatewayRestApiDetails_version' - The version identifier for the REST API.
--
-- 'id', 'awsApiGatewayRestApiDetails_id' - The identifier of the REST API.
--
-- 'endpointConfiguration', 'awsApiGatewayRestApiDetails_endpointConfiguration' - The endpoint configuration of the REST API.
--
-- 'description', 'awsApiGatewayRestApiDetails_description' - A description of the REST API.
newAwsApiGatewayRestApiDetails ::
  AwsApiGatewayRestApiDetails
newAwsApiGatewayRestApiDetails =
  AwsApiGatewayRestApiDetails'
    { minimumCompressionSize =
        Prelude.Nothing,
      binaryMediaTypes = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      name = Prelude.Nothing,
      apiKeySource = Prelude.Nothing,
      version = Prelude.Nothing,
      id = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The minimum size in bytes of a payload before compression is enabled.
--
-- If @null@, then compression is disabled.
--
-- If 0, then all payloads are compressed.
awsApiGatewayRestApiDetails_minimumCompressionSize :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Int)
awsApiGatewayRestApiDetails_minimumCompressionSize = Lens.lens (\AwsApiGatewayRestApiDetails' {minimumCompressionSize} -> minimumCompressionSize) (\s@AwsApiGatewayRestApiDetails' {} a -> s {minimumCompressionSize = a} :: AwsApiGatewayRestApiDetails)

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

-- | The name of the REST API.
awsApiGatewayRestApiDetails_name :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_name = Lens.lens (\AwsApiGatewayRestApiDetails' {name} -> name) (\s@AwsApiGatewayRestApiDetails' {} a -> s {name = a} :: AwsApiGatewayRestApiDetails)

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

-- | The version identifier for the REST API.
awsApiGatewayRestApiDetails_version :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_version = Lens.lens (\AwsApiGatewayRestApiDetails' {version} -> version) (\s@AwsApiGatewayRestApiDetails' {} a -> s {version = a} :: AwsApiGatewayRestApiDetails)

-- | The identifier of the REST API.
awsApiGatewayRestApiDetails_id :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_id = Lens.lens (\AwsApiGatewayRestApiDetails' {id} -> id) (\s@AwsApiGatewayRestApiDetails' {} a -> s {id = a} :: AwsApiGatewayRestApiDetails)

-- | The endpoint configuration of the REST API.
awsApiGatewayRestApiDetails_endpointConfiguration :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe AwsApiGatewayEndpointConfiguration)
awsApiGatewayRestApiDetails_endpointConfiguration = Lens.lens (\AwsApiGatewayRestApiDetails' {endpointConfiguration} -> endpointConfiguration) (\s@AwsApiGatewayRestApiDetails' {} a -> s {endpointConfiguration = a} :: AwsApiGatewayRestApiDetails)

-- | A description of the REST API.
awsApiGatewayRestApiDetails_description :: Lens.Lens' AwsApiGatewayRestApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayRestApiDetails_description = Lens.lens (\AwsApiGatewayRestApiDetails' {description} -> description) (\s@AwsApiGatewayRestApiDetails' {} a -> s {description = a} :: AwsApiGatewayRestApiDetails)

instance Core.FromJSON AwsApiGatewayRestApiDetails where
  parseJSON =
    Core.withObject
      "AwsApiGatewayRestApiDetails"
      ( \x ->
          AwsApiGatewayRestApiDetails'
            Prelude.<$> (x Core..:? "MinimumCompressionSize")
            Prelude.<*> ( x Core..:? "BinaryMediaTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ApiKeySource")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EndpointConfiguration")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable AwsApiGatewayRestApiDetails where
  hashWithSalt salt' AwsApiGatewayRestApiDetails' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` apiKeySource
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` binaryMediaTypes
      `Prelude.hashWithSalt` minimumCompressionSize

instance Prelude.NFData AwsApiGatewayRestApiDetails where
  rnf AwsApiGatewayRestApiDetails' {..} =
    Prelude.rnf minimumCompressionSize
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf apiKeySource
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf binaryMediaTypes

instance Core.ToJSON AwsApiGatewayRestApiDetails where
  toJSON AwsApiGatewayRestApiDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinimumCompressionSize" Core..=)
              Prelude.<$> minimumCompressionSize,
            ("BinaryMediaTypes" Core..=)
              Prelude.<$> binaryMediaTypes,
            ("CreatedDate" Core..=) Prelude.<$> createdDate,
            ("Name" Core..=) Prelude.<$> name,
            ("ApiKeySource" Core..=) Prelude.<$> apiKeySource,
            ("Version" Core..=) Prelude.<$> version,
            ("Id" Core..=) Prelude.<$> id,
            ("EndpointConfiguration" Core..=)
              Prelude.<$> endpointConfiguration,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
