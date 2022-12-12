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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessEndpointPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Verified Access endpoint policy.
module Amazonka.EC2.ModifyVerifiedAccessEndpointPolicy
  ( -- * Creating a Request
    ModifyVerifiedAccessEndpointPolicy (..),
    newModifyVerifiedAccessEndpointPolicy,

    -- * Request Lenses
    modifyVerifiedAccessEndpointPolicy_clientToken,
    modifyVerifiedAccessEndpointPolicy_dryRun,
    modifyVerifiedAccessEndpointPolicy_policyDocument,
    modifyVerifiedAccessEndpointPolicy_verifiedAccessEndpointId,
    modifyVerifiedAccessEndpointPolicy_policyEnabled,

    -- * Destructuring the Response
    ModifyVerifiedAccessEndpointPolicyResponse (..),
    newModifyVerifiedAccessEndpointPolicyResponse,

    -- * Response Lenses
    modifyVerifiedAccessEndpointPolicyResponse_policyDocument,
    modifyVerifiedAccessEndpointPolicyResponse_policyEnabled,
    modifyVerifiedAccessEndpointPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessEndpointPolicy' smart constructor.
data ModifyVerifiedAccessEndpointPolicy = ModifyVerifiedAccessEndpointPolicy'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointId :: Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpointPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessEndpointPolicy_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'modifyVerifiedAccessEndpointPolicy_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'policyDocument', 'modifyVerifiedAccessEndpointPolicy_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'verifiedAccessEndpointId', 'modifyVerifiedAccessEndpointPolicy_verifiedAccessEndpointId' - The ID of the Amazon Web Services Verified Access endpoint.
--
-- 'policyEnabled', 'modifyVerifiedAccessEndpointPolicy_policyEnabled' - The status of the Verified Access policy.
newModifyVerifiedAccessEndpointPolicy ::
  -- | 'verifiedAccessEndpointId'
  Prelude.Text ->
  -- | 'policyEnabled'
  Prelude.Bool ->
  ModifyVerifiedAccessEndpointPolicy
newModifyVerifiedAccessEndpointPolicy
  pVerifiedAccessEndpointId_
  pPolicyEnabled_ =
    ModifyVerifiedAccessEndpointPolicy'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        policyDocument = Prelude.Nothing,
        verifiedAccessEndpointId =
          pVerifiedAccessEndpointId_,
        policyEnabled = pPolicyEnabled_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessEndpointPolicy_clientToken :: Lens.Lens' ModifyVerifiedAccessEndpointPolicy (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpointPolicy_clientToken = Lens.lens (\ModifyVerifiedAccessEndpointPolicy' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessEndpointPolicy' {} a -> s {clientToken = a} :: ModifyVerifiedAccessEndpointPolicy)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessEndpointPolicy_dryRun :: Lens.Lens' ModifyVerifiedAccessEndpointPolicy (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessEndpointPolicy_dryRun = Lens.lens (\ModifyVerifiedAccessEndpointPolicy' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessEndpointPolicy' {} a -> s {dryRun = a} :: ModifyVerifiedAccessEndpointPolicy)

-- | The Amazon Web Services Verified Access policy document.
modifyVerifiedAccessEndpointPolicy_policyDocument :: Lens.Lens' ModifyVerifiedAccessEndpointPolicy (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpointPolicy_policyDocument = Lens.lens (\ModifyVerifiedAccessEndpointPolicy' {policyDocument} -> policyDocument) (\s@ModifyVerifiedAccessEndpointPolicy' {} a -> s {policyDocument = a} :: ModifyVerifiedAccessEndpointPolicy)

-- | The ID of the Amazon Web Services Verified Access endpoint.
modifyVerifiedAccessEndpointPolicy_verifiedAccessEndpointId :: Lens.Lens' ModifyVerifiedAccessEndpointPolicy Prelude.Text
modifyVerifiedAccessEndpointPolicy_verifiedAccessEndpointId = Lens.lens (\ModifyVerifiedAccessEndpointPolicy' {verifiedAccessEndpointId} -> verifiedAccessEndpointId) (\s@ModifyVerifiedAccessEndpointPolicy' {} a -> s {verifiedAccessEndpointId = a} :: ModifyVerifiedAccessEndpointPolicy)

-- | The status of the Verified Access policy.
modifyVerifiedAccessEndpointPolicy_policyEnabled :: Lens.Lens' ModifyVerifiedAccessEndpointPolicy Prelude.Bool
modifyVerifiedAccessEndpointPolicy_policyEnabled = Lens.lens (\ModifyVerifiedAccessEndpointPolicy' {policyEnabled} -> policyEnabled) (\s@ModifyVerifiedAccessEndpointPolicy' {} a -> s {policyEnabled = a} :: ModifyVerifiedAccessEndpointPolicy)

instance
  Core.AWSRequest
    ModifyVerifiedAccessEndpointPolicy
  where
  type
    AWSResponse ModifyVerifiedAccessEndpointPolicy =
      ModifyVerifiedAccessEndpointPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessEndpointPolicyResponse'
            Prelude.<$> (x Data..@? "policyDocument")
              Prelude.<*> (x Data..@? "policyEnabled")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessEndpointPolicy
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessEndpointPolicy' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` policyDocument
        `Prelude.hashWithSalt` verifiedAccessEndpointId
        `Prelude.hashWithSalt` policyEnabled

instance
  Prelude.NFData
    ModifyVerifiedAccessEndpointPolicy
  where
  rnf ModifyVerifiedAccessEndpointPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf verifiedAccessEndpointId
      `Prelude.seq` Prelude.rnf policyEnabled

instance
  Data.ToHeaders
    ModifyVerifiedAccessEndpointPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVerifiedAccessEndpointPolicy
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVerifiedAccessEndpointPolicy
  where
  toQuery ModifyVerifiedAccessEndpointPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVerifiedAccessEndpointPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "PolicyDocument" Data.=: policyDocument,
        "VerifiedAccessEndpointId"
          Data.=: verifiedAccessEndpointId,
        "PolicyEnabled" Data.=: policyEnabled
      ]

-- | /See:/ 'newModifyVerifiedAccessEndpointPolicyResponse' smart constructor.
data ModifyVerifiedAccessEndpointPolicyResponse = ModifyVerifiedAccessEndpointPolicyResponse'
  { -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpointPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'modifyVerifiedAccessEndpointPolicyResponse_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'policyEnabled', 'modifyVerifiedAccessEndpointPolicyResponse_policyEnabled' - The status of the Verified Access policy.
--
-- 'httpStatus', 'modifyVerifiedAccessEndpointPolicyResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessEndpointPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessEndpointPolicyResponse
newModifyVerifiedAccessEndpointPolicyResponse
  pHttpStatus_ =
    ModifyVerifiedAccessEndpointPolicyResponse'
      { policyDocument =
          Prelude.Nothing,
        policyEnabled = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Web Services Verified Access policy document.
modifyVerifiedAccessEndpointPolicyResponse_policyDocument :: Lens.Lens' ModifyVerifiedAccessEndpointPolicyResponse (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpointPolicyResponse_policyDocument = Lens.lens (\ModifyVerifiedAccessEndpointPolicyResponse' {policyDocument} -> policyDocument) (\s@ModifyVerifiedAccessEndpointPolicyResponse' {} a -> s {policyDocument = a} :: ModifyVerifiedAccessEndpointPolicyResponse)

-- | The status of the Verified Access policy.
modifyVerifiedAccessEndpointPolicyResponse_policyEnabled :: Lens.Lens' ModifyVerifiedAccessEndpointPolicyResponse (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessEndpointPolicyResponse_policyEnabled = Lens.lens (\ModifyVerifiedAccessEndpointPolicyResponse' {policyEnabled} -> policyEnabled) (\s@ModifyVerifiedAccessEndpointPolicyResponse' {} a -> s {policyEnabled = a} :: ModifyVerifiedAccessEndpointPolicyResponse)

-- | The response's http status code.
modifyVerifiedAccessEndpointPolicyResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessEndpointPolicyResponse Prelude.Int
modifyVerifiedAccessEndpointPolicyResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessEndpointPolicyResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessEndpointPolicyResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessEndpointPolicyResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessEndpointPolicyResponse
  where
  rnf ModifyVerifiedAccessEndpointPolicyResponse' {..} =
    Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyEnabled
      `Prelude.seq` Prelude.rnf httpStatus
