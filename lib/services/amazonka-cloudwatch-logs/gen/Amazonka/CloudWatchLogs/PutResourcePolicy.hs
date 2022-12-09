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
-- Module      : Amazonka.CloudWatchLogs.PutResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy allowing other Amazon Web Services
-- services to put log events to this account, such as Amazon Route 53. An
-- account can have up to 10 resource policies per Amazon Web Services
-- Region.
module Amazonka.CloudWatchLogs.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policyDocument,
    putResourcePolicy_policyName,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Details of the new policy, including the identity of the principal that
    -- is enabled to put logs to this account. This is formatted as a JSON
    -- string. This parameter is required.
    --
    -- The following example creates a resource policy enabling the Route 53
    -- service to put DNS query logs in to the specified log group. Replace
    -- @\"logArn\"@ with the ARN of your CloudWatch Logs resource, such as a
    -- log group or log stream.
    --
    -- CloudWatch Logs also supports
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourcearn aws:SourceArn>
    -- and
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourceaccount aws:SourceAccount>
    -- condition context keys.
    --
    -- In the example resource policy, you would replace the value of
    -- @SourceArn@ with the resource making the call from Route 53 to
    -- CloudWatch Logs. You would also replace the value of @SourceAccount@
    -- with the Amazon Web Services account ID making that call.
    --
    -- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\": \"logs:PutLogEvents\", \"Resource\": \"logArn\", \"Condition\": { \"ArnLike\": { \"aws:SourceArn\": \"myRoute53ResourceArn\" }, \"StringEquals\": { \"aws:SourceAccount\": \"myAwsAccountId\" } } } ] }@
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | Name of the new policy. This parameter is required.
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDocument', 'putResourcePolicy_policyDocument' - Details of the new policy, including the identity of the principal that
-- is enabled to put logs to this account. This is formatted as a JSON
-- string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53
-- service to put DNS query logs in to the specified log group. Replace
-- @\"logArn\"@ with the ARN of your CloudWatch Logs resource, such as a
-- log group or log stream.
--
-- CloudWatch Logs also supports
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourcearn aws:SourceArn>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourceaccount aws:SourceAccount>
-- condition context keys.
--
-- In the example resource policy, you would replace the value of
-- @SourceArn@ with the resource making the call from Route 53 to
-- CloudWatch Logs. You would also replace the value of @SourceAccount@
-- with the Amazon Web Services account ID making that call.
--
-- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\": \"logs:PutLogEvents\", \"Resource\": \"logArn\", \"Condition\": { \"ArnLike\": { \"aws:SourceArn\": \"myRoute53ResourceArn\" }, \"StringEquals\": { \"aws:SourceAccount\": \"myAwsAccountId\" } } } ] }@
--
-- 'policyName', 'putResourcePolicy_policyName' - Name of the new policy. This parameter is required.
newPutResourcePolicy ::
  PutResourcePolicy
newPutResourcePolicy =
  PutResourcePolicy'
    { policyDocument =
        Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | Details of the new policy, including the identity of the principal that
-- is enabled to put logs to this account. This is formatted as a JSON
-- string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53
-- service to put DNS query logs in to the specified log group. Replace
-- @\"logArn\"@ with the ARN of your CloudWatch Logs resource, such as a
-- log group or log stream.
--
-- CloudWatch Logs also supports
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourcearn aws:SourceArn>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#condition-keys-sourceaccount aws:SourceAccount>
-- condition context keys.
--
-- In the example resource policy, you would replace the value of
-- @SourceArn@ with the resource making the call from Route 53 to
-- CloudWatch Logs. You would also replace the value of @SourceAccount@
-- with the Amazon Web Services account ID making that call.
--
-- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\": \"logs:PutLogEvents\", \"Resource\": \"logArn\", \"Condition\": { \"ArnLike\": { \"aws:SourceArn\": \"myRoute53ResourceArn\" }, \"StringEquals\": { \"aws:SourceAccount\": \"myAwsAccountId\" } } } ] }@
putResourcePolicy_policyDocument :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyDocument = Lens.lens (\PutResourcePolicy' {policyDocument} -> policyDocument) (\s@PutResourcePolicy' {} a -> s {policyDocument = a} :: PutResourcePolicy)

-- | Name of the new policy. This parameter is required.
putResourcePolicy_policyName :: Lens.Lens' PutResourcePolicy (Prelude.Maybe Prelude.Text)
putResourcePolicy_policyName = Lens.lens (\PutResourcePolicy' {policyName} -> policyName) (\s@PutResourcePolicy' {} a -> s {policyName = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Data..?> "resourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("policyDocument" Data..=)
              Prelude.<$> policyDocument,
            ("policyName" Data..=) Prelude.<$> policyName
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The new policy.
    resourcePolicy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePolicy', 'putResourcePolicyResponse_resourcePolicy' - The new policy.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { resourcePolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new policy.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe ResourcePolicy)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
