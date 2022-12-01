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
-- Module      : Amazonka.Redshift.AcceptReservedNodeExchange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exchanges a DC1 Reserved Node for a DC2 Reserved Node with no changes to
-- the configuration (term, payment type, or number of nodes) and no
-- additional costs.
module Amazonka.Redshift.AcceptReservedNodeExchange
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptReservedNodeExchange' smart constructor.
data AcceptReservedNodeExchange = AcceptReservedNodeExchange'
  { -- | A string representing the node identifier of the DC1 Reserved Node to be
    -- exchanged.
    reservedNodeId :: Prelude.Text,
    -- | The unique identifier of the DC2 Reserved Node offering to be used for
    -- the exchange. You can obtain the value for the parameter by calling
    -- GetReservedNodeExchangeOfferings
    targetReservedNodeOfferingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'targetReservedNodeOfferingId'
  Prelude.Text ->
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
acceptReservedNodeExchange_reservedNodeId :: Lens.Lens' AcceptReservedNodeExchange Prelude.Text
acceptReservedNodeExchange_reservedNodeId = Lens.lens (\AcceptReservedNodeExchange' {reservedNodeId} -> reservedNodeId) (\s@AcceptReservedNodeExchange' {} a -> s {reservedNodeId = a} :: AcceptReservedNodeExchange)

-- | The unique identifier of the DC2 Reserved Node offering to be used for
-- the exchange. You can obtain the value for the parameter by calling
-- GetReservedNodeExchangeOfferings
acceptReservedNodeExchange_targetReservedNodeOfferingId :: Lens.Lens' AcceptReservedNodeExchange Prelude.Text
acceptReservedNodeExchange_targetReservedNodeOfferingId = Lens.lens (\AcceptReservedNodeExchange' {targetReservedNodeOfferingId} -> targetReservedNodeOfferingId) (\s@AcceptReservedNodeExchange' {} a -> s {targetReservedNodeOfferingId = a} :: AcceptReservedNodeExchange)

instance Core.AWSRequest AcceptReservedNodeExchange where
  type
    AWSResponse AcceptReservedNodeExchange =
      AcceptReservedNodeExchangeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AcceptReservedNodeExchangeResult"
      ( \s h x ->
          AcceptReservedNodeExchangeResponse'
            Prelude.<$> (x Core..@? "ExchangedReservedNode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptReservedNodeExchange where
  hashWithSalt _salt AcceptReservedNodeExchange' {..} =
    _salt `Prelude.hashWithSalt` reservedNodeId
      `Prelude.hashWithSalt` targetReservedNodeOfferingId

instance Prelude.NFData AcceptReservedNodeExchange where
  rnf AcceptReservedNodeExchange' {..} =
    Prelude.rnf reservedNodeId
      `Prelude.seq` Prelude.rnf targetReservedNodeOfferingId

instance Core.ToHeaders AcceptReservedNodeExchange where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AcceptReservedNodeExchange where
  toPath = Prelude.const "/"

instance Core.ToQuery AcceptReservedNodeExchange where
  toQuery AcceptReservedNodeExchange' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AcceptReservedNodeExchange" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ReservedNodeId" Core.=: reservedNodeId,
        "TargetReservedNodeOfferingId"
          Core.=: targetReservedNodeOfferingId
      ]

-- | /See:/ 'newAcceptReservedNodeExchangeResponse' smart constructor.
data AcceptReservedNodeExchangeResponse = AcceptReservedNodeExchangeResponse'
  { exchangedReservedNode :: Prelude.Maybe ReservedNode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AcceptReservedNodeExchangeResponse
newAcceptReservedNodeExchangeResponse pHttpStatus_ =
  AcceptReservedNodeExchangeResponse'
    { exchangedReservedNode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
acceptReservedNodeExchangeResponse_exchangedReservedNode :: Lens.Lens' AcceptReservedNodeExchangeResponse (Prelude.Maybe ReservedNode)
acceptReservedNodeExchangeResponse_exchangedReservedNode = Lens.lens (\AcceptReservedNodeExchangeResponse' {exchangedReservedNode} -> exchangedReservedNode) (\s@AcceptReservedNodeExchangeResponse' {} a -> s {exchangedReservedNode = a} :: AcceptReservedNodeExchangeResponse)

-- | The response's http status code.
acceptReservedNodeExchangeResponse_httpStatus :: Lens.Lens' AcceptReservedNodeExchangeResponse Prelude.Int
acceptReservedNodeExchangeResponse_httpStatus = Lens.lens (\AcceptReservedNodeExchangeResponse' {httpStatus} -> httpStatus) (\s@AcceptReservedNodeExchangeResponse' {} a -> s {httpStatus = a} :: AcceptReservedNodeExchangeResponse)

instance
  Prelude.NFData
    AcceptReservedNodeExchangeResponse
  where
  rnf AcceptReservedNodeExchangeResponse' {..} =
    Prelude.rnf exchangedReservedNode
      `Prelude.seq` Prelude.rnf httpStatus
