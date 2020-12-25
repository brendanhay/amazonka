{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a resource policy allowing other AWS services to put log events to this account, such as Amazon Route 53. An account can have up to 10 resource policies per AWS Region.
module Network.AWS.CloudWatchLogs.PutResourcePolicy
  ( -- * Creating a request
    PutResourcePolicy (..),
    mkPutResourcePolicy,

    -- ** Request lenses
    prpPolicyDocument,
    prpPolicyName,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprrsResourcePolicy,
    prprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. This parameter is required.
    --
    -- The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace @"logArn"@ with the ARN of your CloudWatch Logs resource, such as a log group or log stream.
    -- @{ "Version": "2012-10-17", "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": "logArn" } ] } @
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | Name of the new policy. This parameter is required.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicy' value with any optional fields omitted.
mkPutResourcePolicy ::
  PutResourcePolicy
mkPutResourcePolicy =
  PutResourcePolicy'
    { policyDocument = Core.Nothing,
      policyName = Core.Nothing
    }

-- | Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace @"logArn"@ with the ARN of your CloudWatch Logs resource, such as a log group or log stream.
-- @{ "Version": "2012-10-17", "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": "logArn" } ] } @
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyDocument :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.PolicyDocument)
prpPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED prpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | Name of the new policy. This parameter is required.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyName :: Lens.Lens' PutResourcePolicy (Core.Maybe Types.PolicyName)
prpPolicyName = Lens.field @"policyName"
{-# DEPRECATED prpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromJSON PutResourcePolicy where
  toJSON PutResourcePolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("policyDocument" Core..=) Core.<$> policyDocument,
            ("policyName" Core..=) Core.<$> policyName
          ]
      )

instance Core.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.PutResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Core.<$> (x Core..:? "resourcePolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The new policy.
    resourcePolicy :: Core.Maybe Types.ResourcePolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutResourcePolicyResponse' value with any optional fields omitted.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse responseStatus =
  PutResourcePolicyResponse'
    { resourcePolicy = Core.Nothing,
      responseStatus
    }

-- | The new policy.
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Core.Maybe Types.ResourcePolicy)
prprrsResourcePolicy = Lens.field @"resourcePolicy"
{-# DEPRECATED prprrsResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprrsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Core.Int
prprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED prprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
