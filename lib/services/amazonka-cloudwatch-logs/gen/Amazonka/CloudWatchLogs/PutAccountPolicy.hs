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
-- Module      : Amazonka.CloudWatchLogs.PutAccountPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an account-level data protection policy that applies to all log
-- groups in the account. A data protection policy can help safeguard
-- sensitive data that\'s ingested by your log groups by auditing and
-- masking the sensitive log data. Each account can have only one
-- account-level policy.
--
-- Sensitive data is detected and masked when it is ingested into a log
-- group. When you set a data protection policy, log events ingested into
-- the log groups before that time are not masked.
--
-- If you use @PutAccountPolicy@ to create a data protection policy for
-- your whole account, it applies to both existing log groups and all log
-- groups that are created later in this account. The account policy is
-- applied to existing log groups with eventual consistency. It might take
-- up to 5 minutes before sensitive data in existing log groups begins to
-- be masked.
--
-- By default, when a user views a log event that includes masked data, the
-- sensitive data is replaced by asterisks. A user who has the
-- @logs:Unmask@ permission can use a
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetLogEvents.html GetLogEvents>
-- or
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_FilterLogEvents.html FilterLogEvents>
-- operation with the @unmask@ parameter set to @true@ to view the unmasked
-- log events. Users with the @logs:Unmask@ can also view unmasked data in
-- the CloudWatch Logs console by running a CloudWatch Logs Insights query
-- with the @unmask@ query command.
--
-- For more information, including a list of types of data that can be
-- audited and masked, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/mask-sensitive-log-data.html Protect sensitive log data with masking>.
--
-- To use the @PutAccountPolicy@ operation, you must be signed on with the
-- @logs:PutDataProtectionPolicy@ and @logs:PutAccountPolicy@ permissions.
--
-- The @PutAccountPolicy@ operation applies to all log groups in the
-- account. You can also use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDataProtectionPolicy.html PutDataProtectionPolicy>
-- to create a data protection policy that applies to just one log group.
-- If a log group has its own data protection policy and the account also
-- has an account-level data protection policy, then the two policies are
-- cumulative. Any sensitive term specified in either policy is masked.
module Amazonka.CloudWatchLogs.PutAccountPolicy
  ( -- * Creating a Request
    PutAccountPolicy (..),
    newPutAccountPolicy,

    -- * Request Lenses
    putAccountPolicy_scope,
    putAccountPolicy_policyName,
    putAccountPolicy_policyDocument,
    putAccountPolicy_policyType,

    -- * Destructuring the Response
    PutAccountPolicyResponse (..),
    newPutAccountPolicyResponse,

    -- * Response Lenses
    putAccountPolicyResponse_accountPolicy,
    putAccountPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAccountPolicy' smart constructor.
data PutAccountPolicy = PutAccountPolicy'
  { -- | Currently the only valid value for this parameter is @GLOBAL@, which
    -- specifies that the data protection policy applies to all log groups in
    -- the account. If you omit this parameter, the default of @GLOBAL@ is
    -- used.
    scope :: Prelude.Maybe Scope,
    -- | A name for the policy. This must be unique within the account.
    policyName :: Prelude.Text,
    -- | Specify the data protection policy, in JSON.
    --
    -- This policy must include two JSON blocks:
    --
    -- -   The first block must include both a @DataIdentifer@ array and an
    --     @Operation@ property with an @Audit@ action. The @DataIdentifer@
    --     array lists the types of sensitive data that you want to mask. For
    --     more information about the available options, see
    --     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/mask-sensitive-log-data-types.html Types of data that you can mask>.
    --
    --     The @Operation@ property with an @Audit@ action is required to find
    --     the sensitive data terms. This @Audit@ action must contain a
    --     @FindingsDestination@ object. You can optionally use that
    --     @FindingsDestination@ object to list one or more destinations to
    --     send audit findings to. If you specify destinations such as log
    --     groups, Kinesis Data Firehose streams, and S3 buckets, they must
    --     already exist.
    --
    -- -   The second block must include both a @DataIdentifer@ array and an
    --     @Operation@ property with an @Deidentify@ action. The
    --     @DataIdentifer@ array must exactly match the @DataIdentifer@ array
    --     in the first block of the policy.
    --
    --     The @Operation@ property with the @Deidentify@ action is what
    --     actually masks the data, and it must contain the
    --     @ \"MaskConfig\": {}@ object. The @ \"MaskConfig\": {}@ object must
    --     be empty.
    --
    -- For an example data protection policy, see the __Examples__ section on
    -- this page.
    --
    -- The contents of the two @DataIdentifer@ arrays must match exactly.
    --
    -- In addition to the two JSON blocks, the @policyDocument@ can also
    -- include @Name@, @Description@, and @Version@ fields. The @Name@ is
    -- different than the operation\'s @policyName@ parameter, and is used as a
    -- dimension when CloudWatch Logs reports audit findings metrics to
    -- CloudWatch.
    --
    -- The JSON specified in @policyDocument@ can be up to 30,720 characters.
    policyDocument :: Prelude.Text,
    -- | Currently the only valid value for this parameter is
    -- @DATA_PROTECTION_POLICY@.
    policyType :: PolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'putAccountPolicy_scope' - Currently the only valid value for this parameter is @GLOBAL@, which
-- specifies that the data protection policy applies to all log groups in
-- the account. If you omit this parameter, the default of @GLOBAL@ is
-- used.
--
-- 'policyName', 'putAccountPolicy_policyName' - A name for the policy. This must be unique within the account.
--
-- 'policyDocument', 'putAccountPolicy_policyDocument' - Specify the data protection policy, in JSON.
--
-- This policy must include two JSON blocks:
--
-- -   The first block must include both a @DataIdentifer@ array and an
--     @Operation@ property with an @Audit@ action. The @DataIdentifer@
--     array lists the types of sensitive data that you want to mask. For
--     more information about the available options, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/mask-sensitive-log-data-types.html Types of data that you can mask>.
--
--     The @Operation@ property with an @Audit@ action is required to find
--     the sensitive data terms. This @Audit@ action must contain a
--     @FindingsDestination@ object. You can optionally use that
--     @FindingsDestination@ object to list one or more destinations to
--     send audit findings to. If you specify destinations such as log
--     groups, Kinesis Data Firehose streams, and S3 buckets, they must
--     already exist.
--
-- -   The second block must include both a @DataIdentifer@ array and an
--     @Operation@ property with an @Deidentify@ action. The
--     @DataIdentifer@ array must exactly match the @DataIdentifer@ array
--     in the first block of the policy.
--
--     The @Operation@ property with the @Deidentify@ action is what
--     actually masks the data, and it must contain the
--     @ \"MaskConfig\": {}@ object. The @ \"MaskConfig\": {}@ object must
--     be empty.
--
-- For an example data protection policy, see the __Examples__ section on
-- this page.
--
-- The contents of the two @DataIdentifer@ arrays must match exactly.
--
-- In addition to the two JSON blocks, the @policyDocument@ can also
-- include @Name@, @Description@, and @Version@ fields. The @Name@ is
-- different than the operation\'s @policyName@ parameter, and is used as a
-- dimension when CloudWatch Logs reports audit findings metrics to
-- CloudWatch.
--
-- The JSON specified in @policyDocument@ can be up to 30,720 characters.
--
-- 'policyType', 'putAccountPolicy_policyType' - Currently the only valid value for this parameter is
-- @DATA_PROTECTION_POLICY@.
newPutAccountPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  -- | 'policyType'
  PolicyType ->
  PutAccountPolicy
newPutAccountPolicy
  pPolicyName_
  pPolicyDocument_
  pPolicyType_ =
    PutAccountPolicy'
      { scope = Prelude.Nothing,
        policyName = pPolicyName_,
        policyDocument = pPolicyDocument_,
        policyType = pPolicyType_
      }

-- | Currently the only valid value for this parameter is @GLOBAL@, which
-- specifies that the data protection policy applies to all log groups in
-- the account. If you omit this parameter, the default of @GLOBAL@ is
-- used.
putAccountPolicy_scope :: Lens.Lens' PutAccountPolicy (Prelude.Maybe Scope)
putAccountPolicy_scope = Lens.lens (\PutAccountPolicy' {scope} -> scope) (\s@PutAccountPolicy' {} a -> s {scope = a} :: PutAccountPolicy)

-- | A name for the policy. This must be unique within the account.
putAccountPolicy_policyName :: Lens.Lens' PutAccountPolicy Prelude.Text
putAccountPolicy_policyName = Lens.lens (\PutAccountPolicy' {policyName} -> policyName) (\s@PutAccountPolicy' {} a -> s {policyName = a} :: PutAccountPolicy)

-- | Specify the data protection policy, in JSON.
--
-- This policy must include two JSON blocks:
--
-- -   The first block must include both a @DataIdentifer@ array and an
--     @Operation@ property with an @Audit@ action. The @DataIdentifer@
--     array lists the types of sensitive data that you want to mask. For
--     more information about the available options, see
--     <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/mask-sensitive-log-data-types.html Types of data that you can mask>.
--
--     The @Operation@ property with an @Audit@ action is required to find
--     the sensitive data terms. This @Audit@ action must contain a
--     @FindingsDestination@ object. You can optionally use that
--     @FindingsDestination@ object to list one or more destinations to
--     send audit findings to. If you specify destinations such as log
--     groups, Kinesis Data Firehose streams, and S3 buckets, they must
--     already exist.
--
-- -   The second block must include both a @DataIdentifer@ array and an
--     @Operation@ property with an @Deidentify@ action. The
--     @DataIdentifer@ array must exactly match the @DataIdentifer@ array
--     in the first block of the policy.
--
--     The @Operation@ property with the @Deidentify@ action is what
--     actually masks the data, and it must contain the
--     @ \"MaskConfig\": {}@ object. The @ \"MaskConfig\": {}@ object must
--     be empty.
--
-- For an example data protection policy, see the __Examples__ section on
-- this page.
--
-- The contents of the two @DataIdentifer@ arrays must match exactly.
--
-- In addition to the two JSON blocks, the @policyDocument@ can also
-- include @Name@, @Description@, and @Version@ fields. The @Name@ is
-- different than the operation\'s @policyName@ parameter, and is used as a
-- dimension when CloudWatch Logs reports audit findings metrics to
-- CloudWatch.
--
-- The JSON specified in @policyDocument@ can be up to 30,720 characters.
putAccountPolicy_policyDocument :: Lens.Lens' PutAccountPolicy Prelude.Text
putAccountPolicy_policyDocument = Lens.lens (\PutAccountPolicy' {policyDocument} -> policyDocument) (\s@PutAccountPolicy' {} a -> s {policyDocument = a} :: PutAccountPolicy)

-- | Currently the only valid value for this parameter is
-- @DATA_PROTECTION_POLICY@.
putAccountPolicy_policyType :: Lens.Lens' PutAccountPolicy PolicyType
putAccountPolicy_policyType = Lens.lens (\PutAccountPolicy' {policyType} -> policyType) (\s@PutAccountPolicy' {} a -> s {policyType = a} :: PutAccountPolicy)

instance Core.AWSRequest PutAccountPolicy where
  type
    AWSResponse PutAccountPolicy =
      PutAccountPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAccountPolicyResponse'
            Prelude.<$> (x Data..?> "accountPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountPolicy where
  hashWithSalt _salt PutAccountPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData PutAccountPolicy where
  rnf PutAccountPolicy' {..} =
    Prelude.rnf scope
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyType

instance Data.ToHeaders PutAccountPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutAccountPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountPolicy where
  toJSON PutAccountPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("scope" Data..=) Prelude.<$> scope,
            Prelude.Just ("policyName" Data..= policyName),
            Prelude.Just
              ("policyDocument" Data..= policyDocument),
            Prelude.Just ("policyType" Data..= policyType)
          ]
      )

instance Data.ToPath PutAccountPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAccountPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountPolicyResponse' smart constructor.
data PutAccountPolicyResponse = PutAccountPolicyResponse'
  { -- | The account policy that you created.
    accountPolicy :: Prelude.Maybe AccountPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountPolicy', 'putAccountPolicyResponse_accountPolicy' - The account policy that you created.
--
-- 'httpStatus', 'putAccountPolicyResponse_httpStatus' - The response's http status code.
newPutAccountPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountPolicyResponse
newPutAccountPolicyResponse pHttpStatus_ =
  PutAccountPolicyResponse'
    { accountPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The account policy that you created.
putAccountPolicyResponse_accountPolicy :: Lens.Lens' PutAccountPolicyResponse (Prelude.Maybe AccountPolicy)
putAccountPolicyResponse_accountPolicy = Lens.lens (\PutAccountPolicyResponse' {accountPolicy} -> accountPolicy) (\s@PutAccountPolicyResponse' {} a -> s {accountPolicy = a} :: PutAccountPolicyResponse)

-- | The response's http status code.
putAccountPolicyResponse_httpStatus :: Lens.Lens' PutAccountPolicyResponse Prelude.Int
putAccountPolicyResponse_httpStatus = Lens.lens (\PutAccountPolicyResponse' {httpStatus} -> httpStatus) (\s@PutAccountPolicyResponse' {} a -> s {httpStatus = a} :: PutAccountPolicyResponse)

instance Prelude.NFData PutAccountPolicyResponse where
  rnf PutAccountPolicyResponse' {..} =
    Prelude.rnf accountPolicy
      `Prelude.seq` Prelude.rnf httpStatus
