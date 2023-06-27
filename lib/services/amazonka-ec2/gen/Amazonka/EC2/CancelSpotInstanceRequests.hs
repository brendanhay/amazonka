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
-- Module      : Amazonka.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot Instance requests.
--
-- Canceling a Spot Instance request does not terminate running Spot
-- Instances associated with the request.
module Amazonka.EC2.CancelSpotInstanceRequests
  ( -- * Creating a Request
    CancelSpotInstanceRequests (..),
    newCancelSpotInstanceRequests,

    -- * Request Lenses
    cancelSpotInstanceRequests_dryRun,
    cancelSpotInstanceRequests_spotInstanceRequestIds,

    -- * Destructuring the Response
    CancelSpotInstanceRequestsResponse (..),
    newCancelSpotInstanceRequestsResponse,

    -- * Response Lenses
    cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests,
    cancelSpotInstanceRequestsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'newCancelSpotInstanceRequests' smart constructor.
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the Spot Instance requests.
    spotInstanceRequestIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotInstanceRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelSpotInstanceRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'spotInstanceRequestIds', 'cancelSpotInstanceRequests_spotInstanceRequestIds' - The IDs of the Spot Instance requests.
newCancelSpotInstanceRequests ::
  CancelSpotInstanceRequests
newCancelSpotInstanceRequests =
  CancelSpotInstanceRequests'
    { dryRun =
        Prelude.Nothing,
      spotInstanceRequestIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelSpotInstanceRequests_dryRun :: Lens.Lens' CancelSpotInstanceRequests (Prelude.Maybe Prelude.Bool)
cancelSpotInstanceRequests_dryRun = Lens.lens (\CancelSpotInstanceRequests' {dryRun} -> dryRun) (\s@CancelSpotInstanceRequests' {} a -> s {dryRun = a} :: CancelSpotInstanceRequests)

-- | The IDs of the Spot Instance requests.
cancelSpotInstanceRequests_spotInstanceRequestIds :: Lens.Lens' CancelSpotInstanceRequests [Prelude.Text]
cancelSpotInstanceRequests_spotInstanceRequestIds = Lens.lens (\CancelSpotInstanceRequests' {spotInstanceRequestIds} -> spotInstanceRequestIds) (\s@CancelSpotInstanceRequests' {} a -> s {spotInstanceRequestIds = a} :: CancelSpotInstanceRequests) Prelude.. Lens.coerced

instance Core.AWSRequest CancelSpotInstanceRequests where
  type
    AWSResponse CancelSpotInstanceRequests =
      CancelSpotInstanceRequestsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotInstanceRequestsResponse'
            Prelude.<$> ( x
                            Data..@? "spotInstanceRequestSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSpotInstanceRequests where
  hashWithSalt _salt CancelSpotInstanceRequests' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` spotInstanceRequestIds

instance Prelude.NFData CancelSpotInstanceRequests where
  rnf CancelSpotInstanceRequests' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf spotInstanceRequestIds

instance Data.ToHeaders CancelSpotInstanceRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelSpotInstanceRequests where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelSpotInstanceRequests where
  toQuery CancelSpotInstanceRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CancelSpotInstanceRequests" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList
          "SpotInstanceRequestId"
          spotInstanceRequestIds
      ]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'newCancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
  { -- | The Spot Instance requests.
    cancelledSpotInstanceRequests :: Prelude.Maybe [CancelledSpotInstanceRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotInstanceRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelledSpotInstanceRequests', 'cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests' - The Spot Instance requests.
--
-- 'httpStatus', 'cancelSpotInstanceRequestsResponse_httpStatus' - The response's http status code.
newCancelSpotInstanceRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelSpotInstanceRequestsResponse
newCancelSpotInstanceRequestsResponse pHttpStatus_ =
  CancelSpotInstanceRequestsResponse'
    { cancelledSpotInstanceRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Spot Instance requests.
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests :: Lens.Lens' CancelSpotInstanceRequestsResponse (Prelude.Maybe [CancelledSpotInstanceRequest])
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests = Lens.lens (\CancelSpotInstanceRequestsResponse' {cancelledSpotInstanceRequests} -> cancelledSpotInstanceRequests) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {cancelledSpotInstanceRequests = a} :: CancelSpotInstanceRequestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
cancelSpotInstanceRequestsResponse_httpStatus :: Lens.Lens' CancelSpotInstanceRequestsResponse Prelude.Int
cancelSpotInstanceRequestsResponse_httpStatus = Lens.lens (\CancelSpotInstanceRequestsResponse' {httpStatus} -> httpStatus) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {httpStatus = a} :: CancelSpotInstanceRequestsResponse)

instance
  Prelude.NFData
    CancelSpotInstanceRequestsResponse
  where
  rnf CancelSpotInstanceRequestsResponse' {..} =
    Prelude.rnf cancelledSpotInstanceRequests
      `Prelude.seq` Prelude.rnf httpStatus
