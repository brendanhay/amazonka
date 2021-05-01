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
-- Module      : Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the verification process to prove that the service provider
-- owns the private DNS name domain for the endpoint service.
--
-- The service provider must successfully perform the verification before
-- the consumer can use the name to access the service.
--
-- Before the service provider runs this command, they must add a record to
-- the DNS server. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html#add-dns-txt-record Adding a TXT Record to Your Domain\'s DNS Server>
-- in the /Amazon VPC User Guide/.
module Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
  ( -- * Creating a Request
    StartVpcEndpointServicePrivateDnsVerification (..),
    newStartVpcEndpointServicePrivateDnsVerification,

    -- * Request Lenses
    startVpcEndpointServicePrivateDnsVerification_dryRun,
    startVpcEndpointServicePrivateDnsVerification_serviceId,

    -- * Destructuring the Response
    StartVpcEndpointServicePrivateDnsVerificationResponse (..),
    newStartVpcEndpointServicePrivateDnsVerificationResponse,

    -- * Response Lenses
    startVpcEndpointServicePrivateDnsVerificationResponse_returnValue,
    startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartVpcEndpointServicePrivateDnsVerification' smart constructor.
data StartVpcEndpointServicePrivateDnsVerification = StartVpcEndpointServicePrivateDnsVerification'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the endpoint service.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartVpcEndpointServicePrivateDnsVerification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'startVpcEndpointServicePrivateDnsVerification_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'startVpcEndpointServicePrivateDnsVerification_serviceId' - The ID of the endpoint service.
newStartVpcEndpointServicePrivateDnsVerification ::
  -- | 'serviceId'
  Prelude.Text ->
  StartVpcEndpointServicePrivateDnsVerification
newStartVpcEndpointServicePrivateDnsVerification
  pServiceId_ =
    StartVpcEndpointServicePrivateDnsVerification'
      { dryRun =
          Prelude.Nothing,
        serviceId = pServiceId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
startVpcEndpointServicePrivateDnsVerification_dryRun :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerification (Prelude.Maybe Prelude.Bool)
startVpcEndpointServicePrivateDnsVerification_dryRun = Lens.lens (\StartVpcEndpointServicePrivateDnsVerification' {dryRun} -> dryRun) (\s@StartVpcEndpointServicePrivateDnsVerification' {} a -> s {dryRun = a} :: StartVpcEndpointServicePrivateDnsVerification)

-- | The ID of the endpoint service.
startVpcEndpointServicePrivateDnsVerification_serviceId :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerification Prelude.Text
startVpcEndpointServicePrivateDnsVerification_serviceId = Lens.lens (\StartVpcEndpointServicePrivateDnsVerification' {serviceId} -> serviceId) (\s@StartVpcEndpointServicePrivateDnsVerification' {} a -> s {serviceId = a} :: StartVpcEndpointServicePrivateDnsVerification)

instance
  Prelude.AWSRequest
    StartVpcEndpointServicePrivateDnsVerification
  where
  type
    Rs StartVpcEndpointServicePrivateDnsVerification =
      StartVpcEndpointServicePrivateDnsVerificationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          StartVpcEndpointServicePrivateDnsVerificationResponse'
            Prelude.<$> (x Prelude..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartVpcEndpointServicePrivateDnsVerification

instance
  Prelude.NFData
    StartVpcEndpointServicePrivateDnsVerification

instance
  Prelude.ToHeaders
    StartVpcEndpointServicePrivateDnsVerification
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    StartVpcEndpointServicePrivateDnsVerification
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    StartVpcEndpointServicePrivateDnsVerification
  where
  toQuery
    StartVpcEndpointServicePrivateDnsVerification' {..} =
      Prelude.mconcat
        [ "Action"
            Prelude.=: ( "StartVpcEndpointServicePrivateDnsVerification" ::
                           Prelude.ByteString
                       ),
          "Version"
            Prelude.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Prelude.=: dryRun,
          "ServiceId" Prelude.=: serviceId
        ]

-- | /See:/ 'newStartVpcEndpointServicePrivateDnsVerificationResponse' smart constructor.
data StartVpcEndpointServicePrivateDnsVerificationResponse = StartVpcEndpointServicePrivateDnsVerificationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartVpcEndpointServicePrivateDnsVerificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'returnValue', 'startVpcEndpointServicePrivateDnsVerificationResponse_returnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus' - The response's http status code.
newStartVpcEndpointServicePrivateDnsVerificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartVpcEndpointServicePrivateDnsVerificationResponse
newStartVpcEndpointServicePrivateDnsVerificationResponse
  pHttpStatus_ =
    StartVpcEndpointServicePrivateDnsVerificationResponse'
      { returnValue =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
startVpcEndpointServicePrivateDnsVerificationResponse_returnValue :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerificationResponse (Prelude.Maybe Prelude.Bool)
startVpcEndpointServicePrivateDnsVerificationResponse_returnValue = Lens.lens (\StartVpcEndpointServicePrivateDnsVerificationResponse' {returnValue} -> returnValue) (\s@StartVpcEndpointServicePrivateDnsVerificationResponse' {} a -> s {returnValue = a} :: StartVpcEndpointServicePrivateDnsVerificationResponse)

-- | The response's http status code.
startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerificationResponse Prelude.Int
startVpcEndpointServicePrivateDnsVerificationResponse_httpStatus = Lens.lens (\StartVpcEndpointServicePrivateDnsVerificationResponse' {httpStatus} -> httpStatus) (\s@StartVpcEndpointServicePrivateDnsVerificationResponse' {} a -> s {httpStatus = a} :: StartVpcEndpointServicePrivateDnsVerificationResponse)

instance
  Prelude.NFData
    StartVpcEndpointServicePrivateDnsVerificationResponse
