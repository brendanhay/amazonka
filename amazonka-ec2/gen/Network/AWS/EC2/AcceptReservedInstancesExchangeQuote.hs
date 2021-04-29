{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The configuration of the target Convertible Reserved Instance to
    -- exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Prelude.Maybe [TargetConfigurationRequest],
    -- | The IDs of the Convertible Reserved Instances to exchange for another
    -- Convertible Reserved Instance of the same or higher value.
    reservedInstanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      targetConfigurations =
        Prelude.Nothing,
      reservedInstanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptReservedInstancesExchangeQuote_dryRun :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Prelude.Maybe Prelude.Bool)
acceptReservedInstancesExchangeQuote_dryRun = Lens.lens (\AcceptReservedInstancesExchangeQuote' {dryRun} -> dryRun) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {dryRun = a} :: AcceptReservedInstancesExchangeQuote)

-- | The configuration of the target Convertible Reserved Instance to
-- exchange for your current Convertible Reserved Instances.
acceptReservedInstancesExchangeQuote_targetConfigurations :: Lens.Lens' AcceptReservedInstancesExchangeQuote (Prelude.Maybe [TargetConfigurationRequest])
acceptReservedInstancesExchangeQuote_targetConfigurations = Lens.lens (\AcceptReservedInstancesExchangeQuote' {targetConfigurations} -> targetConfigurations) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {targetConfigurations = a} :: AcceptReservedInstancesExchangeQuote) Prelude.. Lens.mapping Prelude._Coerce

-- | The IDs of the Convertible Reserved Instances to exchange for another
-- Convertible Reserved Instance of the same or higher value.
acceptReservedInstancesExchangeQuote_reservedInstanceIds :: Lens.Lens' AcceptReservedInstancesExchangeQuote [Prelude.Text]
acceptReservedInstancesExchangeQuote_reservedInstanceIds = Lens.lens (\AcceptReservedInstancesExchangeQuote' {reservedInstanceIds} -> reservedInstanceIds) (\s@AcceptReservedInstancesExchangeQuote' {} a -> s {reservedInstanceIds = a} :: AcceptReservedInstancesExchangeQuote) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AcceptReservedInstancesExchangeQuote
  where
  type
    Rs AcceptReservedInstancesExchangeQuote =
      AcceptReservedInstancesExchangeQuoteResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptReservedInstancesExchangeQuoteResponse'
            Prelude.<$> (x Prelude..@? "exchangeId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptReservedInstancesExchangeQuote

instance
  Prelude.NFData
    AcceptReservedInstancesExchangeQuote

instance
  Prelude.ToHeaders
    AcceptReservedInstancesExchangeQuote
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    AcceptReservedInstancesExchangeQuote
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AcceptReservedInstancesExchangeQuote
  where
  toQuery AcceptReservedInstancesExchangeQuote' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AcceptReservedInstancesExchangeQuote" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          ( Prelude.toQueryList "TargetConfiguration"
              Prelude.<$> targetConfigurations
          ),
        Prelude.toQueryList
          "ReservedInstanceId"
          reservedInstanceIds
      ]

-- | The result of the exchange and whether it was @successful@.
--
-- /See:/ 'newAcceptReservedInstancesExchangeQuoteResponse' smart constructor.
data AcceptReservedInstancesExchangeQuoteResponse = AcceptReservedInstancesExchangeQuoteResponse'
  { -- | The ID of the successful exchange.
    exchangeId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AcceptReservedInstancesExchangeQuoteResponse
newAcceptReservedInstancesExchangeQuoteResponse
  pHttpStatus_ =
    AcceptReservedInstancesExchangeQuoteResponse'
      { exchangeId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the successful exchange.
acceptReservedInstancesExchangeQuoteResponse_exchangeId :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.Text)
acceptReservedInstancesExchangeQuoteResponse_exchangeId = Lens.lens (\AcceptReservedInstancesExchangeQuoteResponse' {exchangeId} -> exchangeId) (\s@AcceptReservedInstancesExchangeQuoteResponse' {} a -> s {exchangeId = a} :: AcceptReservedInstancesExchangeQuoteResponse)

-- | The response's http status code.
acceptReservedInstancesExchangeQuoteResponse_httpStatus :: Lens.Lens' AcceptReservedInstancesExchangeQuoteResponse Prelude.Int
acceptReservedInstancesExchangeQuoteResponse_httpStatus = Lens.lens (\AcceptReservedInstancesExchangeQuoteResponse' {httpStatus} -> httpStatus) (\s@AcceptReservedInstancesExchangeQuoteResponse' {} a -> s {httpStatus = a} :: AcceptReservedInstancesExchangeQuoteResponse)

instance
  Prelude.NFData
    AcceptReservedInstancesExchangeQuoteResponse
