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
-- Module      : Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway multicast domain.
module Network.AWS.EC2.DeleteTransitGatewayMulticastDomain
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTransitGatewayMulticastDomain' smart constructor.
data DeleteTransitGatewayMulticastDomain = DeleteTransitGatewayMulticastDomain'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteTransitGatewayMulticastDomain
newDeleteTransitGatewayMulticastDomain
  pTransitGatewayMulticastDomainId_ =
    DeleteTransitGatewayMulticastDomain'
      { dryRun =
          Core.Nothing,
        transitGatewayMulticastDomainId =
          pTransitGatewayMulticastDomainId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTransitGatewayMulticastDomain_dryRun :: Lens.Lens' DeleteTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
deleteTransitGatewayMulticastDomain_dryRun = Lens.lens (\DeleteTransitGatewayMulticastDomain' {dryRun} -> dryRun) (\s@DeleteTransitGatewayMulticastDomain' {} a -> s {dryRun = a} :: DeleteTransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' DeleteTransitGatewayMulticastDomain Core.Text
deleteTransitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\DeleteTransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@DeleteTransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: DeleteTransitGatewayMulticastDomain)

instance
  Core.AWSRequest
    DeleteTransitGatewayMulticastDomain
  where
  type
    AWSResponse DeleteTransitGatewayMulticastDomain =
      DeleteTransitGatewayMulticastDomainResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "transitGatewayMulticastDomain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteTransitGatewayMulticastDomain

instance
  Core.NFData
    DeleteTransitGatewayMulticastDomain

instance
  Core.ToHeaders
    DeleteTransitGatewayMulticastDomain
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteTransitGatewayMulticastDomain
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteTransitGatewayMulticastDomain
  where
  toQuery DeleteTransitGatewayMulticastDomain' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteTransitGatewayMulticastDomain" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TransitGatewayMulticastDomainId"
          Core.=: transitGatewayMulticastDomainId
      ]

-- | /See:/ 'newDeleteTransitGatewayMulticastDomainResponse' smart constructor.
data DeleteTransitGatewayMulticastDomainResponse = DeleteTransitGatewayMulticastDomainResponse'
  { -- | Information about the deleted transit gateway multicast domain.
    transitGatewayMulticastDomain :: Core.Maybe TransitGatewayMulticastDomain,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteTransitGatewayMulticastDomainResponse
newDeleteTransitGatewayMulticastDomainResponse
  pHttpStatus_ =
    DeleteTransitGatewayMulticastDomainResponse'
      { transitGatewayMulticastDomain =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted transit gateway multicast domain.
deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse (Core.Maybe TransitGatewayMulticastDomain)
deleteTransitGatewayMulticastDomainResponse_transitGatewayMulticastDomain = Lens.lens (\DeleteTransitGatewayMulticastDomainResponse' {transitGatewayMulticastDomain} -> transitGatewayMulticastDomain) (\s@DeleteTransitGatewayMulticastDomainResponse' {} a -> s {transitGatewayMulticastDomain = a} :: DeleteTransitGatewayMulticastDomainResponse)

-- | The response's http status code.
deleteTransitGatewayMulticastDomainResponse_httpStatus :: Lens.Lens' DeleteTransitGatewayMulticastDomainResponse Core.Int
deleteTransitGatewayMulticastDomainResponse_httpStatus = Lens.lens (\DeleteTransitGatewayMulticastDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteTransitGatewayMulticastDomainResponse' {} a -> s {httpStatus = a} :: DeleteTransitGatewayMulticastDomainResponse)

instance
  Core.NFData
    DeleteTransitGatewayMulticastDomainResponse
