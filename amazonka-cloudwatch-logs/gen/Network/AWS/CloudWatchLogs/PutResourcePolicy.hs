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
-- Module      : Network.AWS.CloudWatchLogs.PutResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy allowing other AWS services to put
-- log events to this account, such as Amazon Route 53. An account can have
-- up to 10 resource policies per AWS Region.
module Network.AWS.CloudWatchLogs.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_policyName,
    putResourcePolicy_policyDocument,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Name of the new policy. This parameter is required.
    policyName :: Core.Maybe Core.Text,
    -- | Details of the new policy, including the identity of the principal that
    -- is enabled to put logs to this account. This is formatted as a JSON
    -- string. This parameter is required.
    --
    -- The following example creates a resource policy enabling the Route 53
    -- service to put DNS query logs in to the specified log group. Replace
    -- @\"logArn\"@ with the ARN of your CloudWatch Logs resource, such as a
    -- log group or log stream.
    --
    -- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\":\"logs:PutLogEvents\", \"Resource\": \"logArn\" } ] } @
    policyDocument :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'putResourcePolicy_policyName' - Name of the new policy. This parameter is required.
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
-- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\":\"logs:PutLogEvents\", \"Resource\": \"logArn\" } ] } @
newPutResourcePolicy ::
  PutResourcePolicy
newPutResourcePolicy =
  PutResourcePolicy'
    { policyName = Core.Nothing,
      policyDocument = Core.Nothing
    }

-- | Name of the new policy. This parameter is required.
putResourcePolicy_policyName :: Lens.Lens' PutResourcePolicy (Core.Maybe Core.Text)
putResourcePolicy_policyName = Lens.lens (\PutResourcePolicy' {policyName} -> policyName) (\s@PutResourcePolicy' {} a -> s {policyName = a} :: PutResourcePolicy)

-- | Details of the new policy, including the identity of the principal that
-- is enabled to put logs to this account. This is formatted as a JSON
-- string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53
-- service to put DNS query logs in to the specified log group. Replace
-- @\"logArn\"@ with the ARN of your CloudWatch Logs resource, such as a
-- log group or log stream.
--
-- @{ \"Version\": \"2012-10-17\", \"Statement\": [ { \"Sid\": \"Route53LogsToCloudWatchLogs\", \"Effect\": \"Allow\", \"Principal\": { \"Service\": [ \"route53.amazonaws.com\" ] }, \"Action\":\"logs:PutLogEvents\", \"Resource\": \"logArn\" } ] } @
putResourcePolicy_policyDocument :: Lens.Lens' PutResourcePolicy (Core.Maybe Core.Text)
putResourcePolicy_policyDocument = Lens.lens (\PutResourcePolicy' {policyDocument} -> policyDocument) (\s@PutResourcePolicy' {} a -> s {policyDocument = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Core.<$> (x Core..?> "resourcePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutResourcePolicy

instance Core.NFData PutResourcePolicy

instance Core.ToHeaders PutResourcePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.PutResourcePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("policyName" Core..=) Core.<$> policyName,
            ("policyDocument" Core..=) Core.<$> policyDocument
          ]
      )

instance Core.ToPath PutResourcePolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutResourcePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The new policy.
    resourcePolicy :: Core.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { resourcePolicy =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new policy.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe ResourcePolicy)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Core.NFData PutResourcePolicyResponse
