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
-- Module      : Amazonka.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Amazonka.EC2.DeleteTransitGatewayMulticastDomain
  ( -- * Creating a Request
    DeleteTransitGatewayMulticastDomain (..),
    newDeleteTransitGatewayMulticastDomain,

    -- * Request Lenses
    deleteTransitGatewayMulticastDomain_dryRun,
    deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId,

    -- * Destructuring the Response
    DeleteTransitGatewayMulticastDomainResponse (..),
    newDeleteTransitGatewayMulticastDomainResponse,

    -- * Response Lenses
    deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain,
    deleteTransitGatewayMulticastDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTransitGatewayMulticastDomain_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
newDeleteTransitGatewayMulticastDomain ::
  -- | 'transitGatewayMulticastDomainId'
  Prelude.Text ->
  DeleteTransitGatewayMulticastDomain
newDeleteTransitGatewayMulticastDomain
  pTransitGatewayMulticastDomainId_ =
    DeleteTransitGatewayMulticastDomain'
      { dryRun =
          Prelude.Nothing,
        transitGatewayMulticastDomainId =
          pTransitGatewayMulticastDomainId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayMulticastDomain_dryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Prelude.Maybe Prelude.Bool)
deleteTransitGatewayMulticastDomain_dryRun = Lens.lens (\DeleteTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@DeleteTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: DeleteTransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Prelude.Text
deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\DeleteTransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DeleteTransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: DeleteTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    DeleteTransitGatewayMulticastDomain
  where
  type
    AWSResponse DeleteTransitGatewayMulticastDomain =
      DeleteTransitGatewayMulticastDomainResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayMulticastDomainResponse'
            Prelude.<$> (x Data..@? "transitGatewayMulticastDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTransitGatewayMulticastDomain
  where
  hashWithSalt
    _salt
    DeleteTransitGatewayMulticastDomain' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` transitGatewayMulticastDomainId

instance
  Prelude.NFData
    DeleteTransitGatewayMulticastDomain
  where
  rnf DeleteTransitGatewayMulticastDomain' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId

instance
  Data.ToHeaders
    DeleteTransitGatewayMulticastDomain
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteTransitGatewayMulticastDomain
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteTransitGatewayMulticastDomain
  where
  toQuery DeleteTransitGatewayMulticastDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteTransitGatewayMulticastDomain" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TransitGatewayMulticastDomainId"
          Data.=: transitGatewayMulticastDomainId
      ]

-- | /See:/ 'newDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { -- | Information about the deleted transit gateway multicast domain.
    transitGatewayMulticastDomain :: Prelude.Maybe TransitGatewayMulticastDomain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTransitGatewayMulticastDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayMulticastDomain', 'deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain' - Information about the deleted transit gateway multicast domain.
--
-- 'httpStatus', 'deleteTransitGatewayMulticastDomainResponse_httpStatus' - The response's http status code.
newDeleteTransitGatewayMulticastDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTransitGatewayMulticastDomainResponse
newDeleteTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    DeleteTransitGatewayMulticastDomainResponse'
      { transitGatewayMulticastDomain =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted transit gateway multicast domain.
deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Prelude.Maybe TransitGatewayMulticastDomain)
deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain = Lens.lens (\DeleteTransitGatewayMulticastDomainResponse' {transitGatewayMulticastDomain} -> transitGatewayMulticastDomain) (\s@DeleteTransitGatewayMulticastDomainResponse' {} a -> s {transitGatewayMulticastDomain = a} :: DeleteTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
deleteTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Prelude.Int
deleteTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\DeleteTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayMulticastDomainResponse)

instance
  Prelude.NFData
    DeleteTransitGatewayMulticastDomainResponse
  where
  rnf DeleteTransitGatewayMulticastDomainResponse' {..} =
    Prelude.rnf transitGatewayMulticastDomain
      `Prelude.seq` Prelude.rnf httpStatus
