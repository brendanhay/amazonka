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
-- Module      : Amazonka.CloudWatchLogs.PutDataProtectionPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data protection policy for the specified log group. A data
-- protection policy can help safeguard sensitive data that\'s ingested by
-- the log group by auditing and masking the sensitive log data.
--
-- Sensitive data is detected and masked when it is ingested into the log
-- group. When you set a data protection policy, log events ingested into
-- the log group before that time are not masked.
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
module Amazonka.CloudWatchLogs.PutDataProtectionPolicy
  ( -- * Creating a Request
    PutDataProtectionPolicy (..),
    newPutDataProtectionPolicy,

    -- * Request Lenses
    putDataProtectionPolicy_logGroupIdentifier,
    putDataProtectionPolicy_policyDocument,

    -- * Destructuring the Response
    PutDataProtectionPolicyResponse (..),
    newPutDataProtectionPolicyResponse,

    -- * Response Lenses
    putDataProtectionPolicyResponse_lastUpdatedTime,
    putDataProtectionPolicyResponse_logGroupIdentifier,
    putDataProtectionPolicyResponse_policyDocument,
    putDataProtectionPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDataProtectionPolicy' smart constructor.
data PutDataProtectionPolicy = PutDataProtectionPolicy'
  { -- | Specify either the log group name or log group ARN.
    logGroupIdentifier :: Prelude.Text,
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
    -- The contents of two @DataIdentifer@ arrays must match exactly.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataProtectionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupIdentifier', 'putDataProtectionPolicy_logGroupIdentifier' - Specify either the log group name or log group ARN.
--
-- 'policyDocument', 'putDataProtectionPolicy_policyDocument' - Specify the data protection policy, in JSON.
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
-- The contents of two @DataIdentifer@ arrays must match exactly.
newPutDataProtectionPolicy ::
  -- | 'logGroupIdentifier'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutDataProtectionPolicy
newPutDataProtectionPolicy
  pLogGroupIdentifier_
  pPolicyDocument_ =
    PutDataProtectionPolicy'
      { logGroupIdentifier =
          pLogGroupIdentifier_,
        policyDocument = pPolicyDocument_
      }

-- | Specify either the log group name or log group ARN.
putDataProtectionPolicy_logGroupIdentifier :: Lens.Lens' PutDataProtectionPolicy Prelude.Text
putDataProtectionPolicy_logGroupIdentifier = Lens.lens (\PutDataProtectionPolicy' {logGroupIdentifier} -> logGroupIdentifier) (\s@PutDataProtectionPolicy' {} a -> s {logGroupIdentifier = a} :: PutDataProtectionPolicy)

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
-- The contents of two @DataIdentifer@ arrays must match exactly.
putDataProtectionPolicy_policyDocument :: Lens.Lens' PutDataProtectionPolicy Prelude.Text
putDataProtectionPolicy_policyDocument = Lens.lens (\PutDataProtectionPolicy' {policyDocument} -> policyDocument) (\s@PutDataProtectionPolicy' {} a -> s {policyDocument = a} :: PutDataProtectionPolicy)

instance Core.AWSRequest PutDataProtectionPolicy where
  type
    AWSResponse PutDataProtectionPolicy =
      PutDataProtectionPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDataProtectionPolicyResponse'
            Prelude.<$> (x Data..?> "lastUpdatedTime")
            Prelude.<*> (x Data..?> "logGroupIdentifier")
            Prelude.<*> (x Data..?> "policyDocument")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDataProtectionPolicy where
  hashWithSalt _salt PutDataProtectionPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupIdentifier
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutDataProtectionPolicy where
  rnf PutDataProtectionPolicy' {..} =
    Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders PutDataProtectionPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutDataProtectionPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDataProtectionPolicy where
  toJSON PutDataProtectionPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupIdentifier" Data..= logGroupIdentifier),
            Prelude.Just
              ("policyDocument" Data..= policyDocument)
          ]
      )

instance Data.ToPath PutDataProtectionPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDataProtectionPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDataProtectionPolicyResponse' smart constructor.
data PutDataProtectionPolicyResponse = PutDataProtectionPolicyResponse'
  { -- | The date and time that this policy was most recently updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Natural,
    -- | The log group name or ARN that you specified in your request.
    logGroupIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The data protection policy used for this log group.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDataProtectionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'putDataProtectionPolicyResponse_lastUpdatedTime' - The date and time that this policy was most recently updated.
--
-- 'logGroupIdentifier', 'putDataProtectionPolicyResponse_logGroupIdentifier' - The log group name or ARN that you specified in your request.
--
-- 'policyDocument', 'putDataProtectionPolicyResponse_policyDocument' - The data protection policy used for this log group.
--
-- 'httpStatus', 'putDataProtectionPolicyResponse_httpStatus' - The response's http status code.
newPutDataProtectionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDataProtectionPolicyResponse
newPutDataProtectionPolicyResponse pHttpStatus_ =
  PutDataProtectionPolicyResponse'
    { lastUpdatedTime =
        Prelude.Nothing,
      logGroupIdentifier = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that this policy was most recently updated.
putDataProtectionPolicyResponse_lastUpdatedTime :: Lens.Lens' PutDataProtectionPolicyResponse (Prelude.Maybe Prelude.Natural)
putDataProtectionPolicyResponse_lastUpdatedTime = Lens.lens (\PutDataProtectionPolicyResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@PutDataProtectionPolicyResponse' {} a -> s {lastUpdatedTime = a} :: PutDataProtectionPolicyResponse)

-- | The log group name or ARN that you specified in your request.
putDataProtectionPolicyResponse_logGroupIdentifier :: Lens.Lens' PutDataProtectionPolicyResponse (Prelude.Maybe Prelude.Text)
putDataProtectionPolicyResponse_logGroupIdentifier = Lens.lens (\PutDataProtectionPolicyResponse' {logGroupIdentifier} -> logGroupIdentifier) (\s@PutDataProtectionPolicyResponse' {} a -> s {logGroupIdentifier = a} :: PutDataProtectionPolicyResponse)

-- | The data protection policy used for this log group.
putDataProtectionPolicyResponse_policyDocument :: Lens.Lens' PutDataProtectionPolicyResponse (Prelude.Maybe Prelude.Text)
putDataProtectionPolicyResponse_policyDocument = Lens.lens (\PutDataProtectionPolicyResponse' {policyDocument} -> policyDocument) (\s@PutDataProtectionPolicyResponse' {} a -> s {policyDocument = a} :: PutDataProtectionPolicyResponse)

-- | The response's http status code.
putDataProtectionPolicyResponse_httpStatus :: Lens.Lens' PutDataProtectionPolicyResponse Prelude.Int
putDataProtectionPolicyResponse_httpStatus = Lens.lens (\PutDataProtectionPolicyResponse' {httpStatus} -> httpStatus) (\s@PutDataProtectionPolicyResponse' {} a -> s {httpStatus = a} :: PutDataProtectionPolicyResponse)

instance
  Prelude.NFData
    PutDataProtectionPolicyResponse
  where
  rnf PutDataProtectionPolicyResponse' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf logGroupIdentifier
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf httpStatus
