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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Verified Access group policy.
module Amazonka.EC2.ModifyVerifiedAccessGroupPolicy
  ( -- * Creating a Request
    ModifyVerifiedAccessGroupPolicy (..),
    newModifyVerifiedAccessGroupPolicy,

    -- * Request Lenses
    modifyVerifiedAccessGroupPolicy_clientToken,
    modifyVerifiedAccessGroupPolicy_dryRun,
    modifyVerifiedAccessGroupPolicy_policyDocument,
    modifyVerifiedAccessGroupPolicy_verifiedAccessGroupId,
    modifyVerifiedAccessGroupPolicy_policyEnabled,

    -- * Destructuring the Response
    ModifyVerifiedAccessGroupPolicyResponse (..),
    newModifyVerifiedAccessGroupPolicyResponse,

    -- * Response Lenses
    modifyVerifiedAccessGroupPolicyResponse_policyDocument,
    modifyVerifiedAccessGroupPolicyResponse_policyEnabled,
    modifyVerifiedAccessGroupPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessGroupPolicy' smart constructor.
data ModifyVerifiedAccessGroupPolicy = ModifyVerifiedAccessGroupPolicy'
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
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessGroupPolicy_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'dryRun', 'modifyVerifiedAccessGroupPolicy_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'policyDocument', 'modifyVerifiedAccessGroupPolicy_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'verifiedAccessGroupId', 'modifyVerifiedAccessGroupPolicy_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
--
-- 'policyEnabled', 'modifyVerifiedAccessGroupPolicy_policyEnabled' - The status of the Verified Access policy.
newModifyVerifiedAccessGroupPolicy ::
  -- | 'verifiedAccessGroupId'
  Prelude.Text ->
  -- | 'policyEnabled'
  Prelude.Bool ->
  ModifyVerifiedAccessGroupPolicy
newModifyVerifiedAccessGroupPolicy
  pVerifiedAccessGroupId_
  pPolicyEnabled_ =
    ModifyVerifiedAccessGroupPolicy'
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        policyDocument = Prelude.Nothing,
        verifiedAccessGroupId =
          pVerifiedAccessGroupId_,
        policyEnabled = pPolicyEnabled_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessGroupPolicy_clientToken :: Lens.Lens' ModifyVerifiedAccessGroupPolicy (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroupPolicy_clientToken = Lens.lens (\ModifyVerifiedAccessGroupPolicy' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessGroupPolicy' {} a -> s {clientToken = a} :: ModifyVerifiedAccessGroupPolicy)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessGroupPolicy_dryRun :: Lens.Lens' ModifyVerifiedAccessGroupPolicy (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessGroupPolicy_dryRun = Lens.lens (\ModifyVerifiedAccessGroupPolicy' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessGroupPolicy' {} a -> s {dryRun = a} :: ModifyVerifiedAccessGroupPolicy)

-- | The Amazon Web Services Verified Access policy document.
modifyVerifiedAccessGroupPolicy_policyDocument :: Lens.Lens' ModifyVerifiedAccessGroupPolicy (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroupPolicy_policyDocument = Lens.lens (\ModifyVerifiedAccessGroupPolicy' {policyDocument} -> policyDocument) (\s@ModifyVerifiedAccessGroupPolicy' {} a -> s {policyDocument = a} :: ModifyVerifiedAccessGroupPolicy)

-- | The ID of the Amazon Web Services Verified Access group.
modifyVerifiedAccessGroupPolicy_verifiedAccessGroupId :: Lens.Lens' ModifyVerifiedAccessGroupPolicy Prelude.Text
modifyVerifiedAccessGroupPolicy_verifiedAccessGroupId = Lens.lens (\ModifyVerifiedAccessGroupPolicy' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@ModifyVerifiedAccessGroupPolicy' {} a -> s {verifiedAccessGroupId = a} :: ModifyVerifiedAccessGroupPolicy)

-- | The status of the Verified Access policy.
modifyVerifiedAccessGroupPolicy_policyEnabled :: Lens.Lens' ModifyVerifiedAccessGroupPolicy Prelude.Bool
modifyVerifiedAccessGroupPolicy_policyEnabled = Lens.lens (\ModifyVerifiedAccessGroupPolicy' {policyEnabled} -> policyEnabled) (\s@ModifyVerifiedAccessGroupPolicy' {} a -> s {policyEnabled = a} :: ModifyVerifiedAccessGroupPolicy)

instance
  Core.AWSRequest
    ModifyVerifiedAccessGroupPolicy
  where
  type
    AWSResponse ModifyVerifiedAccessGroupPolicy =
      ModifyVerifiedAccessGroupPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessGroupPolicyResponse'
            Prelude.<$> (x Data..@? "policyDocument")
            Prelude.<*> (x Data..@? "policyEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessGroupPolicy
  where
  hashWithSalt
    _salt
    ModifyVerifiedAccessGroupPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` policyDocument
        `Prelude.hashWithSalt` verifiedAccessGroupId
        `Prelude.hashWithSalt` policyEnabled

instance
  Prelude.NFData
    ModifyVerifiedAccessGroupPolicy
  where
  rnf ModifyVerifiedAccessGroupPolicy' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf policyDocument `Prelude.seq`
          Prelude.rnf verifiedAccessGroupId `Prelude.seq`
            Prelude.rnf policyEnabled

instance
  Data.ToHeaders
    ModifyVerifiedAccessGroupPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVerifiedAccessGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVerifiedAccessGroupPolicy where
  toQuery ModifyVerifiedAccessGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVerifiedAccessGroupPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "PolicyDocument" Data.=: policyDocument,
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId,
        "PolicyEnabled" Data.=: policyEnabled
      ]

-- | /See:/ 'newModifyVerifiedAccessGroupPolicyResponse' smart constructor.
data ModifyVerifiedAccessGroupPolicyResponse = ModifyVerifiedAccessGroupPolicyResponse'
  { -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'modifyVerifiedAccessGroupPolicyResponse_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'policyEnabled', 'modifyVerifiedAccessGroupPolicyResponse_policyEnabled' - The status of the Verified Access policy.
--
-- 'httpStatus', 'modifyVerifiedAccessGroupPolicyResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessGroupPolicyResponse
newModifyVerifiedAccessGroupPolicyResponse
  pHttpStatus_ =
    ModifyVerifiedAccessGroupPolicyResponse'
      { policyDocument =
          Prelude.Nothing,
        policyEnabled = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Web Services Verified Access policy document.
modifyVerifiedAccessGroupPolicyResponse_policyDocument :: Lens.Lens' ModifyVerifiedAccessGroupPolicyResponse (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessGroupPolicyResponse_policyDocument = Lens.lens (\ModifyVerifiedAccessGroupPolicyResponse' {policyDocument} -> policyDocument) (\s@ModifyVerifiedAccessGroupPolicyResponse' {} a -> s {policyDocument = a} :: ModifyVerifiedAccessGroupPolicyResponse)

-- | The status of the Verified Access policy.
modifyVerifiedAccessGroupPolicyResponse_policyEnabled :: Lens.Lens' ModifyVerifiedAccessGroupPolicyResponse (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessGroupPolicyResponse_policyEnabled = Lens.lens (\ModifyVerifiedAccessGroupPolicyResponse' {policyEnabled} -> policyEnabled) (\s@ModifyVerifiedAccessGroupPolicyResponse' {} a -> s {policyEnabled = a} :: ModifyVerifiedAccessGroupPolicyResponse)

-- | The response's http status code.
modifyVerifiedAccessGroupPolicyResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessGroupPolicyResponse Prelude.Int
modifyVerifiedAccessGroupPolicyResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessGroupPolicyResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessGroupPolicyResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessGroupPolicyResponse
  where
  rnf ModifyVerifiedAccessGroupPolicyResponse' {..} =
    Prelude.rnf policyDocument `Prelude.seq`
      Prelude.rnf policyEnabled `Prelude.seq`
        Prelude.rnf httpStatus
