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
-- Module      : Amazonka.APIGateway.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.EndpointConfiguration where

import Amazonka.APIGateway.Types.EndpointType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The endpoint configuration to indicate the types of endpoints an API
-- (RestApi) or its custom domain name (DomainName) has.
--
-- /See:/ 'newEndpointConfiguration' smart constructor.
data EndpointConfiguration = EndpointConfiguration'
  { -- | A list of endpoint types of an API (RestApi) or its custom domain name
    -- (DomainName). For an edge-optimized API and its custom domain name, the
    -- endpoint type is @\"EDGE\"@. For a regional API and its custom domain
    -- name, the endpoint type is @REGIONAL@. For a private API, the endpoint
    -- type is @PRIVATE@.
    types :: Prelude.Maybe [EndpointType],
    -- | A list of VpcEndpointIds of an API (RestApi) against which to create
    -- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
    vpcEndpointIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'types', 'endpointConfiguration_types' - A list of endpoint types of an API (RestApi) or its custom domain name
-- (DomainName). For an edge-optimized API and its custom domain name, the
-- endpoint type is @\"EDGE\"@. For a regional API and its custom domain
-- name, the endpoint type is @REGIONAL@. For a private API, the endpoint
-- type is @PRIVATE@.
--
-- 'vpcEndpointIds', 'endpointConfiguration_vpcEndpointIds' - A list of VpcEndpointIds of an API (RestApi) against which to create
-- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
newEndpointConfiguration ::
  EndpointConfiguration
newEndpointConfiguration =
  EndpointConfiguration'
    { types = Prelude.Nothing,
      vpcEndpointIds = Prelude.Nothing
    }

-- | A list of endpoint types of an API (RestApi) or its custom domain name
-- (DomainName). For an edge-optimized API and its custom domain name, the
-- endpoint type is @\"EDGE\"@. For a regional API and its custom domain
-- name, the endpoint type is @REGIONAL@. For a private API, the endpoint
-- type is @PRIVATE@.
endpointConfiguration_types :: Lens.Lens' EndpointConfiguration (Prelude.Maybe [EndpointType])
endpointConfiguration_types = Lens.lens (\EndpointConfiguration' {types} -> types) (\s@EndpointConfiguration' {} a -> s {types = a} :: EndpointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of VpcEndpointIds of an API (RestApi) against which to create
-- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
endpointConfiguration_vpcEndpointIds :: Lens.Lens' EndpointConfiguration (Prelude.Maybe [Prelude.Text])
endpointConfiguration_vpcEndpointIds = Lens.lens (\EndpointConfiguration' {vpcEndpointIds} -> vpcEndpointIds) (\s@EndpointConfiguration' {} a -> s {vpcEndpointIds = a} :: EndpointConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EndpointConfiguration where
  parseJSON =
    Data.withObject
      "EndpointConfiguration"
      ( \x ->
          EndpointConfiguration'
            Prelude.<$> (x Data..:? "types" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "vpcEndpointIds"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EndpointConfiguration where
  hashWithSalt _salt EndpointConfiguration' {..} =
    _salt `Prelude.hashWithSalt` types
      `Prelude.hashWithSalt` vpcEndpointIds

instance Prelude.NFData EndpointConfiguration where
  rnf EndpointConfiguration' {..} =
    Prelude.rnf types
      `Prelude.seq` Prelude.rnf vpcEndpointIds

instance Data.ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("types" Data..=) Prelude.<$> types,
            ("vpcEndpointIds" Data..=)
              Prelude.<$> vpcEndpointIds
          ]
      )
