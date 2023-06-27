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
-- Module      : Amazonka.CloudWatchLogs.DescribeAccountPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all CloudWatch Logs account policies in the account.
module Amazonka.CloudWatchLogs.DescribeAccountPolicies
  ( -- * Creating a Request
    DescribeAccountPolicies (..),
    newDescribeAccountPolicies,

    -- * Request Lenses
    describeAccountPolicies_accountIdentifiers,
    describeAccountPolicies_policyName,
    describeAccountPolicies_policyType,

    -- * Destructuring the Response
    DescribeAccountPoliciesResponse (..),
    newDescribeAccountPoliciesResponse,

    -- * Response Lenses
    describeAccountPoliciesResponse_accountPolicies,
    describeAccountPoliciesResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountPolicies' smart constructor.
data DescribeAccountPolicies = DescribeAccountPolicies'
  { -- | If you are using an account that is set up as a monitoring account for
    -- CloudWatch unified cross-account observability, you can use this to
    -- specify the account ID of a source account. If you do, the operation
    -- returns the account policy for the specified account. Currently, you can
    -- specify only one account ID in this parameter.
    --
    -- If you omit this parameter, only the policy in the current account is
    -- returned.
    accountIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Use this parameter to limit the returned policies to only the policy
    -- with the name that you specify.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to limit the returned policies to only the policies
    -- that match the policy type that you specify. Currently, the only valid
    -- value is @DATA_PROTECTION_POLICY@.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIdentifiers', 'describeAccountPolicies_accountIdentifiers' - If you are using an account that is set up as a monitoring account for
-- CloudWatch unified cross-account observability, you can use this to
-- specify the account ID of a source account. If you do, the operation
-- returns the account policy for the specified account. Currently, you can
-- specify only one account ID in this parameter.
--
-- If you omit this parameter, only the policy in the current account is
-- returned.
--
-- 'policyName', 'describeAccountPolicies_policyName' - Use this parameter to limit the returned policies to only the policy
-- with the name that you specify.
--
-- 'policyType', 'describeAccountPolicies_policyType' - Use this parameter to limit the returned policies to only the policies
-- that match the policy type that you specify. Currently, the only valid
-- value is @DATA_PROTECTION_POLICY@.
newDescribeAccountPolicies ::
  -- | 'policyType'
  PolicyType ->
  DescribeAccountPolicies
newDescribeAccountPolicies pPolicyType_ =
  DescribeAccountPolicies'
    { accountIdentifiers =
        Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyType = pPolicyType_
    }

-- | If you are using an account that is set up as a monitoring account for
-- CloudWatch unified cross-account observability, you can use this to
-- specify the account ID of a source account. If you do, the operation
-- returns the account policy for the specified account. Currently, you can
-- specify only one account ID in this parameter.
--
-- If you omit this parameter, only the policy in the current account is
-- returned.
describeAccountPolicies_accountIdentifiers :: Lens.Lens' DescribeAccountPolicies (Prelude.Maybe [Prelude.Text])
describeAccountPolicies_accountIdentifiers = Lens.lens (\DescribeAccountPolicies' {accountIdentifiers} -> accountIdentifiers) (\s@DescribeAccountPolicies' {} a -> s {accountIdentifiers = a} :: DescribeAccountPolicies) Prelude.. Lens.mapping Lens.coerced

-- | Use this parameter to limit the returned policies to only the policy
-- with the name that you specify.
describeAccountPolicies_policyName :: Lens.Lens' DescribeAccountPolicies (Prelude.Maybe Prelude.Text)
describeAccountPolicies_policyName = Lens.lens (\DescribeAccountPolicies' {policyName} -> policyName) (\s@DescribeAccountPolicies' {} a -> s {policyName = a} :: DescribeAccountPolicies)

-- | Use this parameter to limit the returned policies to only the policies
-- that match the policy type that you specify. Currently, the only valid
-- value is @DATA_PROTECTION_POLICY@.
describeAccountPolicies_policyType :: Lens.Lens' DescribeAccountPolicies PolicyType
describeAccountPolicies_policyType = Lens.lens (\DescribeAccountPolicies' {policyType} -> policyType) (\s@DescribeAccountPolicies' {} a -> s {policyType = a} :: DescribeAccountPolicies)

instance Core.AWSRequest DescribeAccountPolicies where
  type
    AWSResponse DescribeAccountPolicies =
      DescribeAccountPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAccountPoliciesResponse'
            Prelude.<$> ( x
                            Data..?> "accountPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountPolicies where
  hashWithSalt _salt DescribeAccountPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` accountIdentifiers
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData DescribeAccountPolicies where
  rnf DescribeAccountPolicies' {..} =
    Prelude.rnf accountIdentifiers
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyType

instance Data.ToHeaders DescribeAccountPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeAccountPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAccountPolicies where
  toJSON DescribeAccountPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIdentifiers" Data..=)
              Prelude.<$> accountIdentifiers,
            ("policyName" Data..=) Prelude.<$> policyName,
            Prelude.Just ("policyType" Data..= policyType)
          ]
      )

instance Data.ToPath DescribeAccountPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAccountPoliciesResponse' smart constructor.
data DescribeAccountPoliciesResponse = DescribeAccountPoliciesResponse'
  { -- | An array of structures that contain information about the CloudWatch
    -- Logs account policies that match the specified filters.
    accountPolicies :: Prelude.Maybe [AccountPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountPolicies', 'describeAccountPoliciesResponse_accountPolicies' - An array of structures that contain information about the CloudWatch
-- Logs account policies that match the specified filters.
--
-- 'httpStatus', 'describeAccountPoliciesResponse_httpStatus' - The response's http status code.
newDescribeAccountPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountPoliciesResponse
newDescribeAccountPoliciesResponse pHttpStatus_ =
  DescribeAccountPoliciesResponse'
    { accountPolicies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of structures that contain information about the CloudWatch
-- Logs account policies that match the specified filters.
describeAccountPoliciesResponse_accountPolicies :: Lens.Lens' DescribeAccountPoliciesResponse (Prelude.Maybe [AccountPolicy])
describeAccountPoliciesResponse_accountPolicies = Lens.lens (\DescribeAccountPoliciesResponse' {accountPolicies} -> accountPolicies) (\s@DescribeAccountPoliciesResponse' {} a -> s {accountPolicies = a} :: DescribeAccountPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccountPoliciesResponse_httpStatus :: Lens.Lens' DescribeAccountPoliciesResponse Prelude.Int
describeAccountPoliciesResponse_httpStatus = Lens.lens (\DescribeAccountPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeAccountPoliciesResponse)

instance
  Prelude.NFData
    DescribeAccountPoliciesResponse
  where
  rnf DescribeAccountPoliciesResponse' {..} =
    Prelude.rnf accountPolicies
      `Prelude.seq` Prelude.rnf httpStatus
