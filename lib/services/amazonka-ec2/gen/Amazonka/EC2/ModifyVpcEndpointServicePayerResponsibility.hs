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
-- Module      : Amazonka.EC2.ModifyVpcEndpointServicePayerResponsibility
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the payer responsibility for your VPC endpoint service.
module Amazonka.EC2.ModifyVpcEndpointServicePayerResponsibility
  ( -- * Creating a Request
    ModifyVpcEndpointServicePayerResponsibility (..),
    newModifyVpcEndpointServicePayerResponsibility,

    -- * Request Lenses
    modifyVpcEndpointServicePayerResponsibility_dryRun,
    modifyVpcEndpointServicePayerResponsibility_serviceId,
    modifyVpcEndpointServicePayerResponsibility_payerResponsibility,

    -- * Destructuring the Response
    ModifyVpcEndpointServicePayerResponsibilityResponse (..),
    newModifyVpcEndpointServicePayerResponsibilityResponse,

    -- * Response Lenses
    modifyVpcEndpointServicePayerResponsibilityResponse_returnValue,
    modifyVpcEndpointServicePayerResponsibilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcEndpointServicePayerResponsibility' smart constructor.
data ModifyVpcEndpointServicePayerResponsibility = ModifyVpcEndpointServicePayerResponsibility'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the service.
    serviceId :: Prelude.Text,
    -- | The entity that is responsible for the endpoint costs. The default is
    -- the endpoint owner. If you set the payer responsibility to the service
    -- owner, you cannot set it back to the endpoint owner.
    payerResponsibility :: PayerResponsibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServicePayerResponsibility' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyVpcEndpointServicePayerResponsibility_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'modifyVpcEndpointServicePayerResponsibility_serviceId' - The ID of the service.
--
-- 'payerResponsibility', 'modifyVpcEndpointServicePayerResponsibility_payerResponsibility' - The entity that is responsible for the endpoint costs. The default is
-- the endpoint owner. If you set the payer responsibility to the service
-- owner, you cannot set it back to the endpoint owner.
newModifyVpcEndpointServicePayerResponsibility ::
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'payerResponsibility'
  PayerResponsibility ->
  ModifyVpcEndpointServicePayerResponsibility
newModifyVpcEndpointServicePayerResponsibility
  pServiceId_
  pPayerResponsibility_ =
    ModifyVpcEndpointServicePayerResponsibility'
      { dryRun =
          Prelude.Nothing,
        serviceId = pServiceId_,
        payerResponsibility =
          pPayerResponsibility_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointServicePayerResponsibility_dryRun :: Lens.Lens' ModifyVpcEndpointServicePayerResponsibility (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePayerResponsibility_dryRun = Lens.lens (\ModifyVpcEndpointServicePayerResponsibility' {dryRun} -> dryRun) (\s@ModifyVpcEndpointServicePayerResponsibility' {} a -> s {dryRun = a} :: ModifyVpcEndpointServicePayerResponsibility)

-- | The ID of the service.
modifyVpcEndpointServicePayerResponsibility_serviceId :: Lens.Lens' ModifyVpcEndpointServicePayerResponsibility Prelude.Text
modifyVpcEndpointServicePayerResponsibility_serviceId = Lens.lens (\ModifyVpcEndpointServicePayerResponsibility' {serviceId} -> serviceId) (\s@ModifyVpcEndpointServicePayerResponsibility' {} a -> s {serviceId = a} :: ModifyVpcEndpointServicePayerResponsibility)

-- | The entity that is responsible for the endpoint costs. The default is
-- the endpoint owner. If you set the payer responsibility to the service
-- owner, you cannot set it back to the endpoint owner.
modifyVpcEndpointServicePayerResponsibility_payerResponsibility :: Lens.Lens' ModifyVpcEndpointServicePayerResponsibility PayerResponsibility
modifyVpcEndpointServicePayerResponsibility_payerResponsibility = Lens.lens (\ModifyVpcEndpointServicePayerResponsibility' {payerResponsibility} -> payerResponsibility) (\s@ModifyVpcEndpointServicePayerResponsibility' {} a -> s {payerResponsibility = a} :: ModifyVpcEndpointServicePayerResponsibility)

instance
  Core.AWSRequest
    ModifyVpcEndpointServicePayerResponsibility
  where
  type
    AWSResponse
      ModifyVpcEndpointServicePayerResponsibility =
      ModifyVpcEndpointServicePayerResponsibilityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointServicePayerResponsibilityResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointServicePayerResponsibility
  where
  hashWithSalt
    _salt
    ModifyVpcEndpointServicePayerResponsibility' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` serviceId
        `Prelude.hashWithSalt` payerResponsibility

instance
  Prelude.NFData
    ModifyVpcEndpointServicePayerResponsibility
  where
  rnf ModifyVpcEndpointServicePayerResponsibility' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf payerResponsibility

instance
  Data.ToHeaders
    ModifyVpcEndpointServicePayerResponsibility
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVpcEndpointServicePayerResponsibility
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVpcEndpointServicePayerResponsibility
  where
  toQuery
    ModifyVpcEndpointServicePayerResponsibility' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "ModifyVpcEndpointServicePayerResponsibility" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "ServiceId" Data.=: serviceId,
          "PayerResponsibility" Data.=: payerResponsibility
        ]

-- | /See:/ 'newModifyVpcEndpointServicePayerResponsibilityResponse' smart constructor.
data ModifyVpcEndpointServicePayerResponsibilityResponse = ModifyVpcEndpointServicePayerResponsibilityResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServicePayerResponsibilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'modifyVpcEndpointServicePayerResponsibilityResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyVpcEndpointServicePayerResponsibilityResponse_httpStatus' - The response's http status code.
newModifyVpcEndpointServicePayerResponsibilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpcEndpointServicePayerResponsibilityResponse
newModifyVpcEndpointServicePayerResponsibilityResponse
  pHttpStatus_ =
    ModifyVpcEndpointServicePayerResponsibilityResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointServicePayerResponsibilityResponse_returnValue :: Lens.Lens' ModifyVpcEndpointServicePayerResponsibilityResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServicePayerResponsibilityResponse_returnValue = Lens.lens (\ModifyVpcEndpointServicePayerResponsibilityResponse' {returnValue} -> returnValue) (\s@ModifyVpcEndpointServicePayerResponsibilityResponse' {} a -> s {returnValue = a} :: ModifyVpcEndpointServicePayerResponsibilityResponse)

-- | The response's http status code.
modifyVpcEndpointServicePayerResponsibilityResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointServicePayerResponsibilityResponse Prelude.Int
modifyVpcEndpointServicePayerResponsibilityResponse_httpStatus = Lens.lens (\ModifyVpcEndpointServicePayerResponsibilityResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointServicePayerResponsibilityResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointServicePayerResponsibilityResponse)

instance
  Prelude.NFData
    ModifyVpcEndpointServicePayerResponsibilityResponse
  where
  rnf
    ModifyVpcEndpointServicePayerResponsibilityResponse' {..} =
      Prelude.rnf returnValue
        `Prelude.seq` Prelude.rnf httpStatus
