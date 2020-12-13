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
    prpPolicyName,
    prpPolicyDocument,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprsResourcePolicy,
    prprsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | Name of the new policy. This parameter is required.
    policyName :: Lude.Maybe Lude.Text,
    -- | Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. This parameter is required.
    --
    -- The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace @"logArn"@ with the ARN of your CloudWatch Logs resource, such as a log group or log stream.
    -- @{ "Version": "2012-10-17", "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": "logArn" } ] } @
    policyDocument :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - Name of the new policy. This parameter is required.
-- * 'policyDocument' - Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace @"logArn"@ with the ARN of your CloudWatch Logs resource, such as a log group or log stream.
-- @{ "Version": "2012-10-17", "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": "logArn" } ] } @
mkPutResourcePolicy ::
  PutResourcePolicy
mkPutResourcePolicy =
  PutResourcePolicy'
    { policyName = Lude.Nothing,
      policyDocument = Lude.Nothing
    }

-- | Name of the new policy. This parameter is required.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyName :: Lens.Lens' PutResourcePolicy (Lude.Maybe Lude.Text)
prpPolicyName = Lens.lens (policyName :: PutResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyName = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | Details of the new policy, including the identity of the principal that is enabled to put logs to this account. This is formatted as a JSON string. This parameter is required.
--
-- The following example creates a resource policy enabling the Route 53 service to put DNS query logs in to the specified log group. Replace @"logArn"@ with the ARN of your CloudWatch Logs resource, such as a log group or log stream.
-- @{ "Version": "2012-10-17", "Statement": [ { "Sid": "Route53LogsToCloudWatchLogs", "Effect": "Allow", "Principal": { "Service": [ "route53.amazonaws.com" ] }, "Action":"logs:PutLogEvents", "Resource": "logArn" } ] } @
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyDocument :: Lens.Lens' PutResourcePolicy (Lude.Maybe Lude.Text)
prpPolicyDocument = Lens.lens (policyDocument :: PutResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyDocument = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

instance Lude.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Lude.<$> (x Lude..?> "resourcePolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("policyName" Lude..=) Lude.<$> policyName,
            ("policyDocument" Lude..=) Lude.<$> policyDocument
          ]
      )

instance Lude.ToPath PutResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The new policy.
    resourcePolicy :: Lude.Maybe ResourcePolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'resourcePolicy' - The new policy.
-- * 'responseStatus' - The response status code.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { resourcePolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The new policy.
--
-- /Note:/ Consider using 'resourcePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Lude.Maybe ResourcePolicy)
prprsResourcePolicy = Lens.lens (resourcePolicy :: PutResourcePolicyResponse -> Lude.Maybe ResourcePolicy) (\s a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResourcePolicy "Use generic-lens or generic-optics with 'resourcePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Lude.Int
prprsResponseStatus = Lens.lens (responseStatus :: PutResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
