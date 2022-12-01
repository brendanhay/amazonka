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
-- Module      : Amazonka.GlobalAccelerator.RemoveCustomRoutingEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove endpoints from a custom routing accelerator.
module Amazonka.GlobalAccelerator.RemoveCustomRoutingEndpoints
  ( -- * Creating a Request
    RemoveCustomRoutingEndpoints (..),
    newRemoveCustomRoutingEndpoints,

    -- * Request Lenses
    removeCustomRoutingEndpoints_endpointIds,
    removeCustomRoutingEndpoints_endpointGroupArn,

    -- * Destructuring the Response
    RemoveCustomRoutingEndpointsResponse (..),
    newRemoveCustomRoutingEndpointsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveCustomRoutingEndpoints' smart constructor.
data RemoveCustomRoutingEndpoints = RemoveCustomRoutingEndpoints'
  { -- | The IDs for the endpoints. For custom routing accelerators, endpoint IDs
    -- are the virtual private cloud (VPC) subnet IDs.
    endpointIds :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the endpoint group to remove endpoints
    -- from.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveCustomRoutingEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointIds', 'removeCustomRoutingEndpoints_endpointIds' - The IDs for the endpoints. For custom routing accelerators, endpoint IDs
-- are the virtual private cloud (VPC) subnet IDs.
--
-- 'endpointGroupArn', 'removeCustomRoutingEndpoints_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to remove endpoints
-- from.
newRemoveCustomRoutingEndpoints ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  RemoveCustomRoutingEndpoints
newRemoveCustomRoutingEndpoints pEndpointGroupArn_ =
  RemoveCustomRoutingEndpoints'
    { endpointIds =
        Prelude.mempty,
      endpointGroupArn = pEndpointGroupArn_
    }

-- | The IDs for the endpoints. For custom routing accelerators, endpoint IDs
-- are the virtual private cloud (VPC) subnet IDs.
removeCustomRoutingEndpoints_endpointIds :: Lens.Lens' RemoveCustomRoutingEndpoints [Prelude.Text]
removeCustomRoutingEndpoints_endpointIds = Lens.lens (\RemoveCustomRoutingEndpoints' {endpointIds} -> endpointIds) (\s@RemoveCustomRoutingEndpoints' {} a -> s {endpointIds = a} :: RemoveCustomRoutingEndpoints) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group to remove endpoints
-- from.
removeCustomRoutingEndpoints_endpointGroupArn :: Lens.Lens' RemoveCustomRoutingEndpoints Prelude.Text
removeCustomRoutingEndpoints_endpointGroupArn = Lens.lens (\RemoveCustomRoutingEndpoints' {endpointGroupArn} -> endpointGroupArn) (\s@RemoveCustomRoutingEndpoints' {} a -> s {endpointGroupArn = a} :: RemoveCustomRoutingEndpoints)

instance Core.AWSRequest RemoveCustomRoutingEndpoints where
  type
    AWSResponse RemoveCustomRoutingEndpoints =
      RemoveCustomRoutingEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RemoveCustomRoutingEndpointsResponse'

instance
  Prelude.Hashable
    RemoveCustomRoutingEndpoints
  where
  hashWithSalt _salt RemoveCustomRoutingEndpoints' {..} =
    _salt `Prelude.hashWithSalt` endpointIds
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData RemoveCustomRoutingEndpoints where
  rnf RemoveCustomRoutingEndpoints' {..} =
    Prelude.rnf endpointIds
      `Prelude.seq` Prelude.rnf endpointGroupArn

instance Core.ToHeaders RemoveCustomRoutingEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.RemoveCustomRoutingEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveCustomRoutingEndpoints where
  toJSON RemoveCustomRoutingEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EndpointIds" Core..= endpointIds),
            Prelude.Just
              ("EndpointGroupArn" Core..= endpointGroupArn)
          ]
      )

instance Core.ToPath RemoveCustomRoutingEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveCustomRoutingEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveCustomRoutingEndpointsResponse' smart constructor.
data RemoveCustomRoutingEndpointsResponse = RemoveCustomRoutingEndpointsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveCustomRoutingEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveCustomRoutingEndpointsResponse ::
  RemoveCustomRoutingEndpointsResponse
newRemoveCustomRoutingEndpointsResponse =
  RemoveCustomRoutingEndpointsResponse'

instance
  Prelude.NFData
    RemoveCustomRoutingEndpointsResponse
  where
  rnf _ = ()
