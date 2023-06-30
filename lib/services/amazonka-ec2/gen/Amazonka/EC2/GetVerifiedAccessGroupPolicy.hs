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
-- Module      : Amazonka.EC2.GetVerifiedAccessGroupPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shows the contents of the Verified Access policy associated with the
-- group.
module Amazonka.EC2.GetVerifiedAccessGroupPolicy
  ( -- * Creating a Request
    GetVerifiedAccessGroupPolicy (..),
    newGetVerifiedAccessGroupPolicy,

    -- * Request Lenses
    getVerifiedAccessGroupPolicy_dryRun,
    getVerifiedAccessGroupPolicy_verifiedAccessGroupId,

    -- * Destructuring the Response
    GetVerifiedAccessGroupPolicyResponse (..),
    newGetVerifiedAccessGroupPolicyResponse,

    -- * Response Lenses
    getVerifiedAccessGroupPolicyResponse_policyDocument,
    getVerifiedAccessGroupPolicyResponse_policyEnabled,
    getVerifiedAccessGroupPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVerifiedAccessGroupPolicy' smart constructor.
data GetVerifiedAccessGroupPolicy = GetVerifiedAccessGroupPolicy'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVerifiedAccessGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getVerifiedAccessGroupPolicy_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'verifiedAccessGroupId', 'getVerifiedAccessGroupPolicy_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
newGetVerifiedAccessGroupPolicy ::
  -- | 'verifiedAccessGroupId'
  Prelude.Text ->
  GetVerifiedAccessGroupPolicy
newGetVerifiedAccessGroupPolicy
  pVerifiedAccessGroupId_ =
    GetVerifiedAccessGroupPolicy'
      { dryRun =
          Prelude.Nothing,
        verifiedAccessGroupId =
          pVerifiedAccessGroupId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getVerifiedAccessGroupPolicy_dryRun :: Lens.Lens' GetVerifiedAccessGroupPolicy (Prelude.Maybe Prelude.Bool)
getVerifiedAccessGroupPolicy_dryRun = Lens.lens (\GetVerifiedAccessGroupPolicy' {dryRun} -> dryRun) (\s@GetVerifiedAccessGroupPolicy' {} a -> s {dryRun = a} :: GetVerifiedAccessGroupPolicy)

-- | The ID of the Amazon Web Services Verified Access group.
getVerifiedAccessGroupPolicy_verifiedAccessGroupId :: Lens.Lens' GetVerifiedAccessGroupPolicy Prelude.Text
getVerifiedAccessGroupPolicy_verifiedAccessGroupId = Lens.lens (\GetVerifiedAccessGroupPolicy' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@GetVerifiedAccessGroupPolicy' {} a -> s {verifiedAccessGroupId = a} :: GetVerifiedAccessGroupPolicy)

instance Core.AWSRequest GetVerifiedAccessGroupPolicy where
  type
    AWSResponse GetVerifiedAccessGroupPolicy =
      GetVerifiedAccessGroupPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetVerifiedAccessGroupPolicyResponse'
            Prelude.<$> (x Data..@? "policyDocument")
            Prelude.<*> (x Data..@? "policyEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetVerifiedAccessGroupPolicy
  where
  hashWithSalt _salt GetVerifiedAccessGroupPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` verifiedAccessGroupId

instance Prelude.NFData GetVerifiedAccessGroupPolicy where
  rnf GetVerifiedAccessGroupPolicy' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId

instance Data.ToHeaders GetVerifiedAccessGroupPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVerifiedAccessGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetVerifiedAccessGroupPolicy where
  toQuery GetVerifiedAccessGroupPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetVerifiedAccessGroupPolicy" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId
      ]

-- | /See:/ 'newGetVerifiedAccessGroupPolicyResponse' smart constructor.
data GetVerifiedAccessGroupPolicyResponse = GetVerifiedAccessGroupPolicyResponse'
  { -- | The Amazon Web Services Verified Access policy document.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The status of the Verified Access policy.
    policyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVerifiedAccessGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'getVerifiedAccessGroupPolicyResponse_policyDocument' - The Amazon Web Services Verified Access policy document.
--
-- 'policyEnabled', 'getVerifiedAccessGroupPolicyResponse_policyEnabled' - The status of the Verified Access policy.
--
-- 'httpStatus', 'getVerifiedAccessGroupPolicyResponse_httpStatus' - The response's http status code.
newGetVerifiedAccessGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVerifiedAccessGroupPolicyResponse
newGetVerifiedAccessGroupPolicyResponse pHttpStatus_ =
  GetVerifiedAccessGroupPolicyResponse'
    { policyDocument =
        Prelude.Nothing,
      policyEnabled = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services Verified Access policy document.
getVerifiedAccessGroupPolicyResponse_policyDocument :: Lens.Lens' GetVerifiedAccessGroupPolicyResponse (Prelude.Maybe Prelude.Text)
getVerifiedAccessGroupPolicyResponse_policyDocument = Lens.lens (\GetVerifiedAccessGroupPolicyResponse' {policyDocument} -> policyDocument) (\s@GetVerifiedAccessGroupPolicyResponse' {} a -> s {policyDocument = a} :: GetVerifiedAccessGroupPolicyResponse)

-- | The status of the Verified Access policy.
getVerifiedAccessGroupPolicyResponse_policyEnabled :: Lens.Lens' GetVerifiedAccessGroupPolicyResponse (Prelude.Maybe Prelude.Bool)
getVerifiedAccessGroupPolicyResponse_policyEnabled = Lens.lens (\GetVerifiedAccessGroupPolicyResponse' {policyEnabled} -> policyEnabled) (\s@GetVerifiedAccessGroupPolicyResponse' {} a -> s {policyEnabled = a} :: GetVerifiedAccessGroupPolicyResponse)

-- | The response's http status code.
getVerifiedAccessGroupPolicyResponse_httpStatus :: Lens.Lens' GetVerifiedAccessGroupPolicyResponse Prelude.Int
getVerifiedAccessGroupPolicyResponse_httpStatus = Lens.lens (\GetVerifiedAccessGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@GetVerifiedAccessGroupPolicyResponse' {} a -> s {httpStatus = a} :: GetVerifiedAccessGroupPolicyResponse)

instance
  Prelude.NFData
    GetVerifiedAccessGroupPolicyResponse
  where
  rnf GetVerifiedAccessGroupPolicyResponse' {..} =
    Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyEnabled
      `Prelude.seq` Prelude.rnf httpStatus
