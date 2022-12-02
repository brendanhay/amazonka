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
-- Module      : Amazonka.EC2.GetReservedInstancesExchangeQuote
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a quote and exchange information for exchanging one or more
-- specified Convertible Reserved Instances for a new Convertible Reserved
-- Instance. If the exchange cannot be performed, the reason is returned in
-- the response. Use AcceptReservedInstancesExchangeQuote to perform the
-- exchange.
module Amazonka.EC2.GetReservedInstancesExchangeQuote
  ( -- * Creating a Request
    GetReservedInstancesExchangeQuote (..),
    newGetReservedInstancesExchangeQuote,

    -- * Request Lenses
    getReservedInstancesExchangeQuote_dryRun,
    getReservedInstancesExchangeQuote_targetConfigurations,
    getReservedInstancesExchangeQuote_reservedInstanceIds,

    -- * Destructuring the Response
    GetReservedInstancesExchangeQuoteResponse (..),
    newGetReservedInstancesExchangeQuoteResponse,

    -- * Response Lenses
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup,
    getReservedInstancesExchangeQuoteResponse_validationFailureReason,
    getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt,
    getReservedInstancesExchangeQuoteResponse_currencyCode,
    getReservedInstancesExchangeQuoteResponse_paymentDue,
    getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup,
    getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet,
    getReservedInstancesExchangeQuoteResponse_isValidExchange,
    getReservedInstancesExchangeQuoteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for GetReservedInstanceExchangeQuote.
--
-- /See:/ 'newGetReservedInstancesExchangeQuote' smart constructor.
data GetReservedInstancesExchangeQuote = GetReservedInstancesExchangeQuote'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The configuration of the target Convertible Reserved Instance to
    -- exchange for your current Convertible Reserved Instances.
    targetConfigurations :: Prelude.Maybe [TargetConfigurationRequest],
    -- | The IDs of the Convertible Reserved Instances to exchange.
    reservedInstanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedInstancesExchangeQuote' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getReservedInstancesExchangeQuote_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'targetConfigurations', 'getReservedInstancesExchangeQuote_targetConfigurations' - The configuration of the target Convertible Reserved Instance to
-- exchange for your current Convertible Reserved Instances.
--
-- 'reservedInstanceIds', 'getReservedInstancesExchangeQuote_reservedInstanceIds' - The IDs of the Convertible Reserved Instances to exchange.
newGetReservedInstancesExchangeQuote ::
  GetReservedInstancesExchangeQuote
newGetReservedInstancesExchangeQuote =
  GetReservedInstancesExchangeQuote'
    { dryRun =
        Prelude.Nothing,
      targetConfigurations = Prelude.Nothing,
      reservedInstanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getReservedInstancesExchangeQuote_dryRun :: Lens.Lens' GetReservedInstancesExchangeQuote (Prelude.Maybe Prelude.Bool)
getReservedInstancesExchangeQuote_dryRun = Lens.lens (\GetReservedInstancesExchangeQuote' {dryRun} -> dryRun) (\s@GetReservedInstancesExchangeQuote' {} a -> s {dryRun = a} :: GetReservedInstancesExchangeQuote)

-- | The configuration of the target Convertible Reserved Instance to
-- exchange for your current Convertible Reserved Instances.
getReservedInstancesExchangeQuote_targetConfigurations :: Lens.Lens' GetReservedInstancesExchangeQuote (Prelude.Maybe [TargetConfigurationRequest])
getReservedInstancesExchangeQuote_targetConfigurations = Lens.lens (\GetReservedInstancesExchangeQuote' {targetConfigurations} -> targetConfigurations) (\s@GetReservedInstancesExchangeQuote' {} a -> s {targetConfigurations = a} :: GetReservedInstancesExchangeQuote) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the Convertible Reserved Instances to exchange.
getReservedInstancesExchangeQuote_reservedInstanceIds :: Lens.Lens' GetReservedInstancesExchangeQuote [Prelude.Text]
getReservedInstancesExchangeQuote_reservedInstanceIds = Lens.lens (\GetReservedInstancesExchangeQuote' {reservedInstanceIds} -> reservedInstanceIds) (\s@GetReservedInstancesExchangeQuote' {} a -> s {reservedInstanceIds = a} :: GetReservedInstancesExchangeQuote) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetReservedInstancesExchangeQuote
  where
  type
    AWSResponse GetReservedInstancesExchangeQuote =
      GetReservedInstancesExchangeQuoteResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetReservedInstancesExchangeQuoteResponse'
            Prelude.<$> ( x Data..@? "targetConfigurationValueSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
              Prelude.<*> (x Data..@? "reservedInstanceValueRollup")
              Prelude.<*> (x Data..@? "validationFailureReason")
              Prelude.<*> (x Data..@? "outputReservedInstancesWillExpireAt")
              Prelude.<*> (x Data..@? "currencyCode")
              Prelude.<*> (x Data..@? "paymentDue")
              Prelude.<*> (x Data..@? "targetConfigurationValueRollup")
              Prelude.<*> ( x Data..@? "reservedInstanceValueSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Data.parseXMLList "item")
                          )
              Prelude.<*> (x Data..@? "isValidExchange")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReservedInstancesExchangeQuote
  where
  hashWithSalt
    _salt
    GetReservedInstancesExchangeQuote' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` targetConfigurations
        `Prelude.hashWithSalt` reservedInstanceIds

instance
  Prelude.NFData
    GetReservedInstancesExchangeQuote
  where
  rnf GetReservedInstancesExchangeQuote' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf targetConfigurations
      `Prelude.seq` Prelude.rnf reservedInstanceIds

instance
  Data.ToHeaders
    GetReservedInstancesExchangeQuote
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetReservedInstancesExchangeQuote
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetReservedInstancesExchangeQuote
  where
  toQuery GetReservedInstancesExchangeQuote' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetReservedInstancesExchangeQuote" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TargetConfiguration"
              Prelude.<$> targetConfigurations
          ),
        Data.toQueryList
          "ReservedInstanceId"
          reservedInstanceIds
      ]

-- | Contains the output of GetReservedInstancesExchangeQuote.
--
-- /See:/ 'newGetReservedInstancesExchangeQuoteResponse' smart constructor.
data GetReservedInstancesExchangeQuoteResponse = GetReservedInstancesExchangeQuoteResponse'
  { -- | The values of the target Convertible Reserved Instances.
    targetConfigurationValueSet :: Prelude.Maybe [TargetReservationValue],
    -- | The cost associated with the Reserved Instance.
    reservedInstanceValueRollup :: Prelude.Maybe ReservationValue,
    -- | Describes the reason why the exchange cannot be completed.
    validationFailureReason :: Prelude.Maybe Prelude.Text,
    -- | The new end date of the reservation term.
    outputReservedInstancesWillExpireAt :: Prelude.Maybe Data.ISO8601,
    -- | The currency of the transaction.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The total true upfront charge for the exchange.
    paymentDue :: Prelude.Maybe Prelude.Text,
    -- | The cost associated with the Reserved Instance.
    targetConfigurationValueRollup :: Prelude.Maybe ReservationValue,
    -- | The configuration of your Convertible Reserved Instances.
    reservedInstanceValueSet :: Prelude.Maybe [ReservedInstanceReservationValue],
    -- | If @true@, the exchange is valid. If @false@, the exchange cannot be
    -- completed.
    isValidExchange :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReservedInstancesExchangeQuoteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetConfigurationValueSet', 'getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet' - The values of the target Convertible Reserved Instances.
--
-- 'reservedInstanceValueRollup', 'getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup' - The cost associated with the Reserved Instance.
--
-- 'validationFailureReason', 'getReservedInstancesExchangeQuoteResponse_validationFailureReason' - Describes the reason why the exchange cannot be completed.
--
-- 'outputReservedInstancesWillExpireAt', 'getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt' - The new end date of the reservation term.
--
-- 'currencyCode', 'getReservedInstancesExchangeQuoteResponse_currencyCode' - The currency of the transaction.
--
-- 'paymentDue', 'getReservedInstancesExchangeQuoteResponse_paymentDue' - The total true upfront charge for the exchange.
--
-- 'targetConfigurationValueRollup', 'getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup' - The cost associated with the Reserved Instance.
--
-- 'reservedInstanceValueSet', 'getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet' - The configuration of your Convertible Reserved Instances.
--
-- 'isValidExchange', 'getReservedInstancesExchangeQuoteResponse_isValidExchange' - If @true@, the exchange is valid. If @false@, the exchange cannot be
-- completed.
--
-- 'httpStatus', 'getReservedInstancesExchangeQuoteResponse_httpStatus' - The response's http status code.
newGetReservedInstancesExchangeQuoteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReservedInstancesExchangeQuoteResponse
newGetReservedInstancesExchangeQuoteResponse
  pHttpStatus_ =
    GetReservedInstancesExchangeQuoteResponse'
      { targetConfigurationValueSet =
          Prelude.Nothing,
        reservedInstanceValueRollup =
          Prelude.Nothing,
        validationFailureReason =
          Prelude.Nothing,
        outputReservedInstancesWillExpireAt =
          Prelude.Nothing,
        currencyCode = Prelude.Nothing,
        paymentDue = Prelude.Nothing,
        targetConfigurationValueRollup =
          Prelude.Nothing,
        reservedInstanceValueSet =
          Prelude.Nothing,
        isValidExchange =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The values of the target Convertible Reserved Instances.
getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe [TargetReservationValue])
getReservedInstancesExchangeQuoteResponse_targetConfigurationValueSet = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {targetConfigurationValueSet} -> targetConfigurationValueSet) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {targetConfigurationValueSet = a} :: GetReservedInstancesExchangeQuoteResponse) Prelude.. Lens.mapping Lens.coerced

-- | The cost associated with the Reserved Instance.
getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe ReservationValue)
getReservedInstancesExchangeQuoteResponse_reservedInstanceValueRollup = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {reservedInstanceValueRollup} -> reservedInstanceValueRollup) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {reservedInstanceValueRollup = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | Describes the reason why the exchange cannot be completed.
getReservedInstancesExchangeQuoteResponse_validationFailureReason :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.Text)
getReservedInstancesExchangeQuoteResponse_validationFailureReason = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {validationFailureReason} -> validationFailureReason) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {validationFailureReason = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | The new end date of the reservation term.
getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.UTCTime)
getReservedInstancesExchangeQuoteResponse_outputReservedInstancesWillExpireAt = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {outputReservedInstancesWillExpireAt} -> outputReservedInstancesWillExpireAt) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {outputReservedInstancesWillExpireAt = a} :: GetReservedInstancesExchangeQuoteResponse) Prelude.. Lens.mapping Data._Time

-- | The currency of the transaction.
getReservedInstancesExchangeQuoteResponse_currencyCode :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.Text)
getReservedInstancesExchangeQuoteResponse_currencyCode = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {currencyCode} -> currencyCode) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {currencyCode = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | The total true upfront charge for the exchange.
getReservedInstancesExchangeQuoteResponse_paymentDue :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.Text)
getReservedInstancesExchangeQuoteResponse_paymentDue = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {paymentDue} -> paymentDue) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {paymentDue = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | The cost associated with the Reserved Instance.
getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe ReservationValue)
getReservedInstancesExchangeQuoteResponse_targetConfigurationValueRollup = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {targetConfigurationValueRollup} -> targetConfigurationValueRollup) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {targetConfigurationValueRollup = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | The configuration of your Convertible Reserved Instances.
getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe [ReservedInstanceReservationValue])
getReservedInstancesExchangeQuoteResponse_reservedInstanceValueSet = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {reservedInstanceValueSet} -> reservedInstanceValueSet) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {reservedInstanceValueSet = a} :: GetReservedInstancesExchangeQuoteResponse) Prelude.. Lens.mapping Lens.coerced

-- | If @true@, the exchange is valid. If @false@, the exchange cannot be
-- completed.
getReservedInstancesExchangeQuoteResponse_isValidExchange :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse (Prelude.Maybe Prelude.Bool)
getReservedInstancesExchangeQuoteResponse_isValidExchange = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {isValidExchange} -> isValidExchange) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {isValidExchange = a} :: GetReservedInstancesExchangeQuoteResponse)

-- | The response's http status code.
getReservedInstancesExchangeQuoteResponse_httpStatus :: Lens.Lens' GetReservedInstancesExchangeQuoteResponse Prelude.Int
getReservedInstancesExchangeQuoteResponse_httpStatus = Lens.lens (\GetReservedInstancesExchangeQuoteResponse' {httpStatus} -> httpStatus) (\s@GetReservedInstancesExchangeQuoteResponse' {} a -> s {httpStatus = a} :: GetReservedInstancesExchangeQuoteResponse)

instance
  Prelude.NFData
    GetReservedInstancesExchangeQuoteResponse
  where
  rnf GetReservedInstancesExchangeQuoteResponse' {..} =
    Prelude.rnf targetConfigurationValueSet
      `Prelude.seq` Prelude.rnf reservedInstanceValueRollup
      `Prelude.seq` Prelude.rnf validationFailureReason
      `Prelude.seq` Prelude.rnf outputReservedInstancesWillExpireAt
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf paymentDue
      `Prelude.seq` Prelude.rnf targetConfigurationValueRollup
      `Prelude.seq` Prelude.rnf reservedInstanceValueSet
      `Prelude.seq` Prelude.rnf isValidExchange
      `Prelude.seq` Prelude.rnf httpStatus
