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
-- Module      : Amazonka.EC2.GetVerifiedAccessEndpointPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Verified Access policy associated with the endpoint.
module Amazonka.EC2.GetVerifiedAccessEndpointPolicy
  ( -- * Creating a Request
    GetVerifiedAccessEndpointPolicy (..),
    newGetVerifiedAccessEndpointPolicy,

    -- * Request Lenses
    getVerifiedAccessEndpointPolicy_dryRun,
    getVerifiedAccessEndpointPolicy_verifiedAccessEndpointId,

    -- * Destructuring the Response
    GetVerifiedAccessEndpointPolicyResponse (..),
    newGetVerifiedAccessEndpointPolicyResponse,

    -- * Response Lenses
    getVerifiedAccessEndpointPolicyResponse_policyDocument,
    getVerifiedAccessEndpointPolicyResponse_policyEnabled,
    getVerifiedAccessEndpointPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVerifiedAccessEndpointPolicy' smart constructor.
data GetVerifiedAccessEndpointPolicy = GetVerifiedAccessEndpointPolicy'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVerifiedAccessEndpointPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getVerifiedAccessEndpointPolicy_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessEndpointId', 'getVerifiedAccessEndpointPolicy_verifiedAccessEndpointId' - The ID of the Amazon Web Services Verified Access endpoint.
newGetVerifiedAccessEndpointPolicy ::
  -- | 'verifiedAccessEndpointId'
  Prelude.Text ->
  GetVerifiedAccessEndpointPolicy
newGetVerifiedAccessEndpointPolicy
  pVerifiedAccessEndpointId_ =
    GetVerifiedAccessEndpointPolicy'
      { dryRun =
          Prelude.Nothing,
        verifiedAccessEndpointId =
          pVerifiedAccessEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVerifiedAccessEndpointPolicy_dryRun :: Lens.Lens' GetVerifiedAccessEndpointPolicy (Prelude.Maybe Prelude.Bool)
getVerifiedAccessEndpointPolicy_dryRun = Lens.lens (\GetVerifiedAccessEndpointPolicy' {dryRun} -> dryRun) (\s@GetVerifiedAccessEndpointPolicy' {} a -> s {dryRun = a} :: GetVerifiedAccessEndpointPolicy)

-- | The ID of the Amazon Web Services Verified Access endpoint.
getVerifiedAccessEndpointPolicy_verifiedAccessEndpointId :: Lens.Lens' GetVerifiedAccessEndpointPolicy Prelude.Text
getVerifiedAccessEndpointPolicy_verifiedAccessEndpointId = Lens.lens (\GetVerifiedAccessEndpointPolicy' {verifiedAccessEndpointId} -> verifiedAccessEndpointId) (\s@GetVerifiedAccessEndpointPolicy' {} a -> s {verifiedAccessEndpointId = a} :: GetVerifiedAccessEndpointPolicy)

instance
  Core.AWSRequest
    GetVerifiedAccessEndpointPolicy
  where
  type
    AWSResponse GetVerifiedAccessEndpointPolicy =
      GetVerifiedAccessEndpointPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetVerifiedAccessEndpointPolicyResponse'
            Prelude.<$> (x Data..@? "policyDocument")
            Prelude.<*> (x Data..@? "policyEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVerifiedAccessEndpointPolicy
  where
  hashWithSalt
    _salt
    GetVerifiedAccessEndpointPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` verifiedAccessEndpointId

instance
  Prelude.NFData
    GetVerifiedAccessEndpointPolicy
  where
  rnf GetVerifiedAccessEndpointPolicy' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessEndpointId

instance
  Data.ToHeaders
    GetVerifiedAccessEndpointPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVerifiedAccessEndpointPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVerifiedAccessEndpointPolicy where
  toQuery GetVerifiedAccessEndpointPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetVerifiedAccessEndpointPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VerifiedAccessEndpointId"
          Data.=: verifiedAccessEndpointId
      ]

-- | /See:/ 'newGetVerifiedAccessEndpointPolicyResponse' smart constructor.
data GetVerifiedAccessEndpointPolicyResponse = GetVerifiedAccessEndpointPolicyResponse'
  { -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVerifiedAccessEndpointPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'getVerifiedAccessEndpointPolicyResponse_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'policyEnabled', 'getVerifiedAccessEndpointPolicyResponse_policyEnabled' - The status of the Verified Access policy.
--
-- 'httpStatus', 'getVerifiedAccessEndpointPolicyResponse_httpStatus' - The response's http status code.
newGetVerifiedAccessEndpointPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVerifiedAccessEndpointPolicyResponse
newGetVerifiedAccessEndpointPolicyResponse
  pHttpStatus_ =
    GetVerifiedAccessEndpointPolicyResponse'
      { policyDocument =
          Prelude.Nothing,
        policyEnabled = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Web Services Verified Access policy document.
getVerifiedAccessEndpointPolicyResponse_policyDocument :: Lens.Lens' GetVerifiedAccessEndpointPolicyResponse (Prelude.Maybe Prelude.Text)
getVerifiedAccessEndpointPolicyResponse_policyDocument = Lens.lens (\GetVerifiedAccessEndpointPolicyResponse' {policyDocument} -> policyDocument) (\s@GetVerifiedAccessEndpointPolicyResponse' {} a -> s {policyDocument = a} :: GetVerifiedAccessEndpointPolicyResponse)

-- | The status of the Verified Access policy.
getVerifiedAccessEndpointPolicyResponse_policyEnabled :: Lens.Lens' GetVerifiedAccessEndpointPolicyResponse (Prelude.Maybe Prelude.Bool)
getVerifiedAccessEndpointPolicyResponse_policyEnabled = Lens.lens (\GetVerifiedAccessEndpointPolicyResponse' {policyEnabled} -> policyEnabled) (\s@GetVerifiedAccessEndpointPolicyResponse' {} a -> s {policyEnabled = a} :: GetVerifiedAccessEndpointPolicyResponse)

-- | The response's http status code.
getVerifiedAccessEndpointPolicyResponse_httpStatus :: Lens.Lens' GetVerifiedAccessEndpointPolicyResponse Prelude.Int
getVerifiedAccessEndpointPolicyResponse_httpStatus = Lens.lens (\GetVerifiedAccessEndpointPolicyResponse' {httpStatus} -> httpStatus) (\s@GetVerifiedAccessEndpointPolicyResponse' {} a -> s {httpStatus = a} :: GetVerifiedAccessEndpointPolicyResponse)

instance
  Prelude.NFData
    GetVerifiedAccessEndpointPolicyResponse
  where
  rnf GetVerifiedAccessEndpointPolicyResponse' {..} =
    Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyEnabled
      `Prelude.seq` Prelude.rnf httpStatus
