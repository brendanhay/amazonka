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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    types :: Core.Maybe [EndpointType],
    -- | A list of VpcEndpointIds of an API (RestApi) against which to create
    -- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
    vpcEndpointIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { types = Core.Nothing,
      vpcEndpointIds = Core.Nothing
    }

-- | A list of endpoint types of an API (RestApi) or its custom domain name
-- (DomainName). For an edge-optimized API and its custom domain name, the
-- endpoint type is @\"EDGE\"@. For a regional API and its custom domain
-- name, the endpoint type is @REGIONAL@. For a private API, the endpoint
-- type is @PRIVATE@.
endpointConfiguration_types :: Lens.Lens' EndpointConfiguration (Core.Maybe [EndpointType])
endpointConfiguration_types = Lens.lens (\EndpointConfiguration' {types} -> types) (\s@EndpointConfiguration' {} a -> s {types = a} :: EndpointConfiguration) Core.. Lens.mapping Lens._Coerce

-- | A list of VpcEndpointIds of an API (RestApi) against which to create
-- Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
endpointConfiguration_vpcEndpointIds :: Lens.Lens' EndpointConfiguration (Core.Maybe [Core.Text])
endpointConfiguration_vpcEndpointIds = Lens.lens (\EndpointConfiguration' {vpcEndpointIds} -> vpcEndpointIds) (\s@EndpointConfiguration' {} a -> s {vpcEndpointIds = a} :: EndpointConfiguration) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON EndpointConfiguration where
  parseJSON =
    Core.withObject
      "EndpointConfiguration"
      ( \x ->
          EndpointConfiguration'
            Core.<$> (x Core..:? "types" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpcEndpointIds" Core..!= Core.mempty)
      )

instance Core.Hashable EndpointConfiguration

instance Core.NFData EndpointConfiguration

instance Core.ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("types" Core..=) Core.<$> types,
            ("vpcEndpointIds" Core..=) Core.<$> vpcEndpointIds
          ]
      )
