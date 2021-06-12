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
-- Module      : Network.AWS.Redshift.AcceptReservedNodeExchange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exchanges a DC1 Reserved Node for a DC2 Reserved Node with no changes to
-- the configuration (term, payment type, or number of nodes) and no
-- additional costs.
module Network.AWS.Redshift.AcceptReservedNodeExchange
  ( -- * Creating a Request
    AcceptReservedNodeExchange (..),
    newAcceptReservedNodeExchange,

    -- * Request Lenses
    acceptReservedNodeExchange_reservedNodeId,
    acceptReservedNodeExchange_targetReservedNodeOfferingId,

    -- * Destructuring the Response
    AcceptReservedNodeExchangeResponse (..),
    newAcceptReservedNodeExchangeResponse,

    -- * Response Lenses
    acceptReservedNodeExchangeResponse_exchangedReservedNode,
    acceptReservedNodeExchangeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptReservedNodeExchange' smart constructor.
data AcceptReservedNodeExchange = AcceptReservedNodeExchange'
  { -- | A string representing the node identifier of the DC1 Reserved Node to be
    -- exchanged.
    reservedNodeId :: Core.Text,
    -- | The unique identifier of the DC2 Reserved Node offering to be used for
    -- the exchange. You can obtain the value for the parameter by calling
    -- GetReservedNodeExchangeOfferings
    targetReservedNodeOfferingId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptReservedNodeExchange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeId', 'acceptReservedNodeExchange_reservedNodeId' - A string representing the node identifier of the DC1 Reserved Node to be
-- exchanged.
--
-- 'targetReservedNodeOfferingId', 'acceptReservedNodeExchange_targetReservedNodeOfferingId' - The unique identifier of the DC2 Reserved Node offering to be used for
-- the exchange. You can obtain the value for the parameter by calling
-- GetReservedNodeExchangeOfferings
newAcceptReservedNodeExchange ::
  -- | 'reservedNodeId'
  Core.Text ->
  -- | 'targetReservedNodeOfferingId'
  Core.Text ->
  AcceptReservedNodeExchange
newAcceptReservedNodeExchange
  pReservedNodeId_
  pTargetReservedNodeOfferingId_ =
    AcceptReservedNodeExchange'
      { reservedNodeId =
          pReservedNodeId_,
        targetReservedNodeOfferingId =
          pTargetReservedNodeOfferingId_
      }

-- | A string representing the node identifier of the DC1 Reserved Node to be
-- exchanged.
acceptReservedNodeExchange_reservedNodeId :: Lens.Lens' AcceptReservedNodeExchange Core.Text
acceptReservedNodeExchange_reservedNodeId = Lens.lens (\AcceptReservedNodeExchange' {reservedNodeId} -> reservedNodeId) (\s@AcceptReservedNodeExchange' {} a -> s {reservedNodeId = a} :: AcceptReservedNodeExchange)

-- | The unique identifier of the DC2 Reserved Node offering to be used for
-- the exchange. You can obtain the value for the parameter by calling
-- GetReservedNodeExchangeOfferings
acceptReservedNodeExchange_targetReservedNodeOfferingId :: Lens.Lens' AcceptReservedNodeExchange Core.Text
acceptReservedNodeExchange_targetReservedNodeOfferingId = Lens.lens (\AcceptReservedNodeExchange' {targetReservedNodeOfferingId} -> targetReservedNodeOfferingId) (\s@AcceptReservedNodeExchange' {} a -> s {targetReservedNodeOfferingId = a} :: AcceptReservedNodeExchange)

instance Core.AWSRequest AcceptReservedNodeExchange where
  type
    AWSResponse AcceptReservedNodeExchange =
      AcceptReservedNodeExchangeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AcceptReservedNodeExchangeResult"
      ( \s h x ->
          AcceptReservedNodeExchangeResponse'
            Core.<$> (x Core..@? "ExchangedReservedNode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptReservedNodeExchange

instance Core.NFData AcceptReservedNodeExchange

instance Core.ToHeaders AcceptReservedNodeExchange where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AcceptReservedNodeExchange where
  toPath = Core.const "/"

instance Core.ToQuery AcceptReservedNodeExchange where
  toQuery AcceptReservedNodeExchange' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AcceptReservedNodeExchange" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ReservedNodeId" Core.=: reservedNodeId,
        "TargetReservedNodeOfferingId"
          Core.=: targetReservedNodeOfferingId
      ]

-- | /See:/ 'newAcceptReservedNodeExchangeResponse' smart constructor.
data AcceptReservedNodeExchangeResponse = AcceptReservedNodeExchangeResponse'
  { exchangedReservedNode :: Core.Maybe ReservedNode,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptReservedNodeExchangeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exchangedReservedNode', 'acceptReservedNodeExchangeResponse_exchangedReservedNode' -
--
-- 'httpStatus', 'acceptReservedNodeExchangeResponse_httpStatus' - The response's http status code.
newAcceptReservedNodeExchangeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptReservedNodeExchangeResponse
newAcceptReservedNodeExchangeResponse pHttpStatus_ =
  AcceptReservedNodeExchangeResponse'
    { exchangedReservedNode =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
acceptReservedNodeExchangeResponse_exchangedReservedNode :: Lens.Lens' AcceptReservedNodeExchangeResponse (Core.Maybe ReservedNode)
acceptReservedNodeExchangeResponse_exchangedReservedNode = Lens.lens (\AcceptReservedNodeExchangeResponse' {exchangedReservedNode} -> exchangedReservedNode) (\s@AcceptReservedNodeExchangeResponse' {} a -> s {exchangedReservedNode = a} :: AcceptReservedNodeExchangeResponse)

-- | The response's http status code.
acceptReservedNodeExchangeResponse_httpStatus :: Lens.Lens' AcceptReservedNodeExchangeResponse Core.Int
acceptReservedNodeExchangeResponse_httpStatus = Lens.lens (\AcceptReservedNodeExchangeResponse' {httpStatus} -> httpStatus) (\s@AcceptReservedNodeExchangeResponse' {} a -> s {httpStatus = a} :: AcceptReservedNodeExchangeResponse)

instance
  Core.NFData
    AcceptReservedNodeExchangeResponse
