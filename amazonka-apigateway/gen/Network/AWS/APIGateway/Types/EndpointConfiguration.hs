{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointConfiguration where

import Network.AWS.APIGateway.Types.EndpointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
endpointConfiguration_types = Lens.lens (\EndpointConfiguration' {types} -> types) (\s@EndpointConfiguration' {} a -> s {types = a} :: EndpointConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of VpcEndpointIds of an API (RestApi) against which to create
-- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
endpointConfiguration_vpcEndpointIds :: Lens.Lens' EndpointConfiguration (Prelude.Maybe [Prelude.Text])
endpointConfiguration_vpcEndpointIds = Lens.lens (\EndpointConfiguration' {vpcEndpointIds} -> vpcEndpointIds) (\s@EndpointConfiguration' {} a -> s {vpcEndpointIds = a} :: EndpointConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON EndpointConfiguration where
  parseJSON =
    Prelude.withObject
      "EndpointConfiguration"
      ( \x ->
          EndpointConfiguration'
            Prelude.<$> (x Prelude..:? "types" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "vpcEndpointIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EndpointConfiguration

instance Prelude.NFData EndpointConfiguration

instance Prelude.ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("types" Prelude..=) Prelude.<$> types,
            ("vpcEndpointIds" Prelude..=)
              Prelude.<$> vpcEndpointIds
          ]
      )
