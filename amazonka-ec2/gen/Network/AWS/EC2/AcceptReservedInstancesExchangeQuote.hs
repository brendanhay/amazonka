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
-- Module      : Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the Convertible Reserved Instance exchange quote described in
-- the GetReservedInstancesExchangeQuote call.
module Network.AWS.EC2.AcceptReservedInstancesExchangeQuote
  ( -- * Creating a Request
    AcceptReservedInstancesExchangeQuote (..),
    newAcceptReservedInstancesExchangeQuote,

    -- * Request Lenses
    acceptReservedInstancesExchangeQuote_dryRun,
    acceptReservedInstancesExchangeQuote_targetConfigurations,
    acceptReservedInstancesExchangeQuote_reservedInstanceIds,

    -- * Destructuring the Response
    AcceptReservedInstancesExchangeQuoteResponse (..),
    newAcceptReservedInstancesExchangeQuoteResponse,

    -- * Response Lenses
    acceptReservedInstancesExchangeQuoteResponse_exchangeId,
    acceptReservedInstancesExchangeQuoteResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for accepting the quote.
--
-- /See:/ 'newAcceptReservedInstancesExchangeQuote' smart constructor.
data AcceptReservedInstancesExchangeQuote = AcceptReservedInstancesExchangeQuote'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The configuration of the target Convertible Reserved Instance to
    -- exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Core.Maybe [TargetConfigurationRequest],
    -- | The IDs of the Convertible Reserved Instances to exchange for another
    -- Convertible Reserved Instance of the same or higher value.
    reservedInstanceIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptReservedInstancesExchangeQuote' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptReservedInstancesExchangeQuote_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'targetConfigurations', 'acceptReservedInstancesExchangeQuote_targetConfigurations' - The configuration of the target Convertible Reserved Instance to
-- exchange for your current Convertible Reserved Instances.
--
-- 'reservedInstanceIds', 'acceptReservedInstancesExchangeQuote_reservedInstanceIds' - The IDs of the Convertible Reserved Instances to exchange for another
-- Convertible Reserved Instance of the same or higher value.
newAcceptReservedInstancesExchangeQuote ::
  AcceptReservedInstancesExchangeQuote
newAcceptReservedInstancesExchangeQuote =
  AcceptReservedInstancesExchangeQuote'
    { dryRun =
        Core.Nothing,
      targetConfigurations = Core.Nothing,
      reservedInstanceIds = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptReservedInstancesExchangeQuote_dryRun :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Core.Maybe Core.Bool)
acceptReservedInstancesExchangeQuote_dryRun = Lens.lens (\AcceptReservedInstancesExchangeQuote' {dryRun} -> dryRun) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {dryRun = a} :: AcceptReservedInstancesExchangeQuote)

-- | The configuration of the target Convertible Reserved Instance to
-- exchange for your current Convertible Reserved Instances.
acceptReservedInstancesExchangeQuote_targetConfigurations :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Core.Maybe [TargetConfigurationRequest])
acceptReservedInstancesExchangeQuote_targetConfigurations = Lens.lens (\AcceptReservedInstancesExchangeQuote' {targetConfigurations} -> targetConfigurations) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {targetConfigurations = a} :: AcceptReservedInstancesExchangeQuote) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the Convertible Reserved Instances to exchange for another
-- Convertible Reserved Instance of the same or higher value.
acceptReservedInstancesExchangeQuote_reservedInstanceIds :: Lens.Lens' AcceptReservedInstancesExchangeQuote [Core.Text]
acceptReservedInstancesExchangeQuote_reservedInstanceIds = Lens.lens (\AcceptReservedInstancesExchangeQuote' {reservedInstanceIds} -> reservedInstanceIds) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {reservedInstanceIds = a} :: AcceptReservedInstancesExchangeQuote) Core.. Lens._Coerce

instance
  Core.AWSRequest
    AcceptReservedInstancesExchangeQuote
  where
  type
    AWSResponse AcceptReservedInstancesExchangeQuote =
      AcceptReservedInstancesExchangeQuoteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptReservedInstancesExchangeQuoteResponse'
            Core.<$> (x Core..@? "exchangeId")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    AcceptReservedInstancesExchangeQuote

instance
  Core.NFData
    AcceptReservedInstancesExchangeQuote

instance
  Core.ToHeaders
    AcceptReservedInstancesExchangeQuote
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    AcceptReservedInstancesExchangeQuote
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    AcceptReservedInstancesExchangeQuote
  where
  toQuery AcceptReservedInstancesExchangeQuote' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AcceptReservedInstancesExchangeQuote" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TargetConfiguration"
              Core.<$> targetConfigurations
          ),
        Core.toQueryList
          "ReservedInstanceId"
          reservedInstanceIds
      ]

-- | The result of the exchange and whether it was @successful@.
--
-- /See:/ 'newAcceptReservedInstancesExchangeQuoteResponse' smart constructor.
data AcceptReservedInstancesExchangeQuoteResponse = AcceptReservedInstancesExchangeQuoteResponse'
  { -- | The ID of the successful exchange.
    exchangeId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptReservedInstancesExchangeQuoteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exchangeId', 'acceptReservedInstancesExchangeQuoteResponse_exchangeId' - The ID of the successful exchange.
--
-- 'httpStatus', 'acceptReservedInstancesExchangeQuoteResponse_httpStatus' - The response's http status code.
newAcceptReservedInstancesExchangeQuoteResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptReservedInstancesExchangeQuoteResponse
newAcceptReservedInstancesExchangeQuoteResponse
  pHttpStatus_ =
    AcceptReservedInstancesExchangeQuoteResponse'
      { exchangeId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the successful exchange.
acceptReservedInstancesExchangeQuoteResponse_exchangeId :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse (Core.Maybe Core.Text)
acceptReservedInstancesExchangeQuoteResponse_exchangeId = Lens.lens (\AcceptReservedInstancesExchangeQuoteResponse' {exchangeId} -> exchangeId) (\s@AcceptReservedInstancesExchangeQuoteResponse' {} a -> s {exchangeId = a} :: AcceptReservedInstancesExchangeQuoteResponse)

-- | The response's http status code.
acceptReservedInstancesExchangeQuoteResponse_httpStatus :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse Core.Int
acceptReservedInstancesExchangeQuoteResponse_httpStatus = Lens.lens (\AcceptReservedInstancesExchangeQuoteResponse' {httpStatus} -> httpStatus) (\s@AcceptReservedInstancesExchangeQuoteResponse' {} a -> s {httpStatus = a} :: AcceptReservedInstancesExchangeQuoteResponse)

instance
  Core.NFData
    AcceptReservedInstancesExchangeQuoteResponse
