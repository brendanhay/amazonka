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
-- Module      : Amazonka.Transfer.DescribeSecurityPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the security policy that is attached to your file transfer
-- protocol-enabled server. The response contains a description of the
-- security policy\'s properties. For more information about security
-- policies, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/security-policies.html Working with security policies>.
module Amazonka.Transfer.DescribeSecurityPolicy
  ( -- * Creating a Request
    DescribeSecurityPolicy (..),
    newDescribeSecurityPolicy,

    -- * Request Lenses
    describeSecurityPolicy_securityPolicyName,

    -- * Destructuring the Response
    DescribeSecurityPolicyResponse (..),
    newDescribeSecurityPolicyResponse,

    -- * Response Lenses
    describeSecurityPolicyResponse_httpStatus,
    describeSecurityPolicyResponse_securityPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeSecurityPolicy' smart constructor.
data DescribeSecurityPolicy = DescribeSecurityPolicy'
  { -- | Specifies the name of the security policy that is attached to the
    -- server.
    securityPolicyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityPolicyName', 'describeSecurityPolicy_securityPolicyName' - Specifies the name of the security policy that is attached to the
-- server.
newDescribeSecurityPolicy ::
  -- | 'securityPolicyName'
  Prelude.Text ->
  DescribeSecurityPolicy
newDescribeSecurityPolicy pSecurityPolicyName_ =
  DescribeSecurityPolicy'
    { securityPolicyName =
        pSecurityPolicyName_
    }

-- | Specifies the name of the security policy that is attached to the
-- server.
describeSecurityPolicy_securityPolicyName :: Lens.Lens' DescribeSecurityPolicy Prelude.Text
describeSecurityPolicy_securityPolicyName = Lens.lens (\DescribeSecurityPolicy' {securityPolicyName} -> securityPolicyName) (\s@DescribeSecurityPolicy' {} a -> s {securityPolicyName = a} :: DescribeSecurityPolicy)

instance Core.AWSRequest DescribeSecurityPolicy where
  type
    AWSResponse DescribeSecurityPolicy =
      DescribeSecurityPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSecurityPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SecurityPolicy")
      )

instance Prelude.Hashable DescribeSecurityPolicy where
  hashWithSalt _salt DescribeSecurityPolicy' {..} =
    _salt `Prelude.hashWithSalt` securityPolicyName

instance Prelude.NFData DescribeSecurityPolicy where
  rnf DescribeSecurityPolicy' {..} =
    Prelude.rnf securityPolicyName

instance Data.ToHeaders DescribeSecurityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DescribeSecurityPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSecurityPolicy where
  toJSON DescribeSecurityPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityPolicyName" Data..= securityPolicyName)
          ]
      )

instance Data.ToPath DescribeSecurityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSecurityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSecurityPolicyResponse' smart constructor.
data DescribeSecurityPolicyResponse = DescribeSecurityPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array containing the properties of the security policy.
    securityPolicy :: DescribedSecurityPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeSecurityPolicyResponse_httpStatus' - The response's http status code.
--
-- 'securityPolicy', 'describeSecurityPolicyResponse_securityPolicy' - An array containing the properties of the security policy.
newDescribeSecurityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'securityPolicy'
  DescribedSecurityPolicy ->
  DescribeSecurityPolicyResponse
newDescribeSecurityPolicyResponse
  pHttpStatus_
  pSecurityPolicy_ =
    DescribeSecurityPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        securityPolicy = pSecurityPolicy_
      }

-- | The response's http status code.
describeSecurityPolicyResponse_httpStatus :: Lens.Lens' DescribeSecurityPolicyResponse Prelude.Int
describeSecurityPolicyResponse_httpStatus = Lens.lens (\DescribeSecurityPolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityPolicyResponse' {} a -> s {httpStatus = a} :: DescribeSecurityPolicyResponse)

-- | An array containing the properties of the security policy.
describeSecurityPolicyResponse_securityPolicy :: Lens.Lens' DescribeSecurityPolicyResponse DescribedSecurityPolicy
describeSecurityPolicyResponse_securityPolicy = Lens.lens (\DescribeSecurityPolicyResponse' {securityPolicy} -> securityPolicy) (\s@DescribeSecurityPolicyResponse' {} a -> s {securityPolicy = a} :: DescribeSecurityPolicyResponse)

instance
  Prelude.NFData
    DescribeSecurityPolicyResponse
  where
  rnf DescribeSecurityPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf securityPolicy
