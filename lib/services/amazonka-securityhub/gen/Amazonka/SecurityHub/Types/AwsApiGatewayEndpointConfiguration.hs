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
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayEndpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the endpoints for the API.
--
-- /See:/ 'newAwsApiGatewayEndpointConfiguration' smart constructor.
data AwsApiGatewayEndpointConfiguration = AwsApiGatewayEndpointConfiguration'
  { -- | A list of endpoint types for the REST API.
    --
    -- For an edge-optimized API, the endpoint type is @EDGE@. For a Regional
    -- API, the endpoint type is @REGIONAL@. For a private API, the endpoint
    -- type is @PRIVATE@.
    types :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'types', 'awsApiGatewayEndpointConfiguration_types' - A list of endpoint types for the REST API.
--
-- For an edge-optimized API, the endpoint type is @EDGE@. For a Regional
-- API, the endpoint type is @REGIONAL@. For a private API, the endpoint
-- type is @PRIVATE@.
newAwsApiGatewayEndpointConfiguration ::
  AwsApiGatewayEndpointConfiguration
newAwsApiGatewayEndpointConfiguration =
  AwsApiGatewayEndpointConfiguration'
    { types =
        Prelude.Nothing
    }

-- | A list of endpoint types for the REST API.
--
-- For an edge-optimized API, the endpoint type is @EDGE@. For a Regional
-- API, the endpoint type is @REGIONAL@. For a private API, the endpoint
-- type is @PRIVATE@.
awsApiGatewayEndpointConfiguration_types :: Lens.Lens' AwsApiGatewayEndpointConfiguration (Prelude.Maybe [Prelude.Text])
awsApiGatewayEndpointConfiguration_types = Lens.lens (\AwsApiGatewayEndpointConfiguration' {types} -> types) (\s@AwsApiGatewayEndpointConfiguration' {} a -> s {types = a} :: AwsApiGatewayEndpointConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsApiGatewayEndpointConfiguration
  where
  parseJSON =
    Data.withObject
      "AwsApiGatewayEndpointConfiguration"
      ( \x ->
          AwsApiGatewayEndpointConfiguration'
            Prelude.<$> (x Data..:? "Types" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsApiGatewayEndpointConfiguration
  where
  hashWithSalt
    _salt
    AwsApiGatewayEndpointConfiguration' {..} =
      _salt `Prelude.hashWithSalt` types

instance
  Prelude.NFData
    AwsApiGatewayEndpointConfiguration
  where
  rnf AwsApiGatewayEndpointConfiguration' {..} =
    Prelude.rnf types

instance
  Data.ToJSON
    AwsApiGatewayEndpointConfiguration
  where
  toJSON AwsApiGatewayEndpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Types" Data..=) Prelude.<$> types]
      )
