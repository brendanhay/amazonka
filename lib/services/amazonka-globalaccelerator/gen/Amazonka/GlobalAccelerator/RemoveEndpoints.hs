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
-- Module      : Amazonka.GlobalAccelerator.RemoveEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove endpoints from an endpoint group.
--
-- The @RemoveEndpoints@ API operation is the recommended option for
-- removing endpoints. The alternative is to remove endpoints by updating
-- an endpoint group by using the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_UpdateEndpointGroup.html UpdateEndpointGroup>
-- API operation. There are two advantages to using @AddEndpoints@ to
-- remove endpoints instead:
--
-- -   It\'s more convenient, because you only need to specify the
--     endpoints that you want to remove. With the @UpdateEndpointGroup@
--     API operation, you must specify all of the endpoints in the endpoint
--     group except the ones that you want to remove from the group.
--
-- -   It\'s faster, because Global Accelerator doesn\'t need to resolve
--     any endpoints. With the @UpdateEndpointGroup@ API operation, Global
--     Accelerator must resolve all of the endpoints that remain in the
--     group.
module Amazonka.GlobalAccelerator.RemoveEndpoints
  ( -- * Creating a Request
    RemoveEndpoints (..),
    newRemoveEndpoints,

    -- * Request Lenses
    removeEndpoints_endpointIdentifiers,
    removeEndpoints_endpointGroupArn,

    -- * Destructuring the Response
    RemoveEndpointsResponse (..),
    newRemoveEndpointsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveEndpoints' smart constructor.
data RemoveEndpoints = RemoveEndpoints'
  { -- | The identifiers of the endpoints that you want to remove.
    endpointIdentifiers :: Prelude.NonEmpty EndpointIdentifier,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointIdentifiers', 'removeEndpoints_endpointIdentifiers' - The identifiers of the endpoints that you want to remove.
--
-- 'endpointGroupArn', 'removeEndpoints_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
newRemoveEndpoints ::
  -- | 'endpointIdentifiers'
  Prelude.NonEmpty EndpointIdentifier ->
  -- | 'endpointGroupArn'
  Prelude.Text ->
  RemoveEndpoints
newRemoveEndpoints
  pEndpointIdentifiers_
  pEndpointGroupArn_ =
    RemoveEndpoints'
      { endpointIdentifiers =
          Lens.coerced Lens.# pEndpointIdentifiers_,
        endpointGroupArn = pEndpointGroupArn_
      }

-- | The identifiers of the endpoints that you want to remove.
removeEndpoints_endpointIdentifiers :: Lens.Lens' RemoveEndpoints (Prelude.NonEmpty EndpointIdentifier)
removeEndpoints_endpointIdentifiers = Lens.lens (\RemoveEndpoints' {endpointIdentifiers} -> endpointIdentifiers) (\s@RemoveEndpoints' {} a -> s {endpointIdentifiers = a} :: RemoveEndpoints) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
removeEndpoints_endpointGroupArn :: Lens.Lens' RemoveEndpoints Prelude.Text
removeEndpoints_endpointGroupArn = Lens.lens (\RemoveEndpoints' {endpointGroupArn} -> endpointGroupArn) (\s@RemoveEndpoints' {} a -> s {endpointGroupArn = a} :: RemoveEndpoints)

instance Core.AWSRequest RemoveEndpoints where
  type
    AWSResponse RemoveEndpoints =
      RemoveEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RemoveEndpointsResponse'

instance Prelude.Hashable RemoveEndpoints where
  hashWithSalt _salt RemoveEndpoints' {..} =
    _salt `Prelude.hashWithSalt` endpointIdentifiers
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData RemoveEndpoints where
  rnf RemoveEndpoints' {..} =
    Prelude.rnf endpointIdentifiers
      `Prelude.seq` Prelude.rnf endpointGroupArn

instance Data.ToHeaders RemoveEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.RemoveEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveEndpoints where
  toJSON RemoveEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointIdentifiers" Data..= endpointIdentifiers),
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath RemoveEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveEndpointsResponse' smart constructor.
data RemoveEndpointsResponse = RemoveEndpointsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveEndpointsResponse ::
  RemoveEndpointsResponse
newRemoveEndpointsResponse = RemoveEndpointsResponse'

instance Prelude.NFData RemoveEndpointsResponse where
  rnf _ = ()
