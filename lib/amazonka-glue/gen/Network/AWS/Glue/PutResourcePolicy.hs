{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Data Catalog resource policy for access control.
module Network.AWS.Glue.PutResourcePolicy
  ( -- * Creating a request
    PutResourcePolicy (..),
    mkPutResourcePolicy,

    -- ** Request lenses
    prpPolicyExistsCondition,
    prpPolicyHashCondition,
    prpResourceARN,
    prpEnableHybrid,
    prpPolicyInJSON,

    -- * Destructuring the response
    PutResourcePolicyResponse (..),
    mkPutResourcePolicyResponse,

    -- ** Response lenses
    prprsPolicyHash,
    prprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { policyExistsCondition ::
      Lude.Maybe ExistCondition,
    policyHashCondition :: Lude.Maybe Lude.Text,
    resourceARN :: Lude.Maybe Lude.Text,
    enableHybrid :: Lude.Maybe EnableHybridValues,
    policyInJSON :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicy' with the minimum fields required to make a request.
--
-- * 'enableHybrid' - Allows you to specify if you want to use both resource-level and account/catalog-level resource policies. A resource-level policy is a policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot co-exist with an account-level policy. A value of @YES@ means the use of both resource-level and account/catalog-level resource policies is allowed.
-- * 'policyExistsCondition' - A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
-- * 'policyHashCondition' - The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
-- * 'policyInJSON' - Contains the policy document to set, in JSON format.
-- * 'resourceARN' - The ARN of the AWS Glue resource for the resource policy to be set. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
mkPutResourcePolicy ::
  -- | 'policyInJSON'
  Lude.Text ->
  PutResourcePolicy
mkPutResourcePolicy pPolicyInJSON_ =
  PutResourcePolicy'
    { policyExistsCondition = Lude.Nothing,
      policyHashCondition = Lude.Nothing,
      resourceARN = Lude.Nothing,
      enableHybrid = Lude.Nothing,
      policyInJSON = pPolicyInJSON_
    }

-- | A value of @MUST_EXIST@ is used to update a policy. A value of @NOT_EXIST@ is used to create a new policy. If a value of @NONE@ or a null value is used, the call will not depend on the existence of a policy.
--
-- /Note:/ Consider using 'policyExistsCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyExistsCondition :: Lens.Lens' PutResourcePolicy (Lude.Maybe ExistCondition)
prpPolicyExistsCondition = Lens.lens (policyExistsCondition :: PutResourcePolicy -> Lude.Maybe ExistCondition) (\s a -> s {policyExistsCondition = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicyExistsCondition "Use generic-lens or generic-optics with 'policyExistsCondition' instead." #-}

-- | The hash value returned when the previous policy was set using @PutResourcePolicy@ . Its purpose is to prevent concurrent modifications of a policy. Do not use this parameter if no previous policy has been set.
--
-- /Note:/ Consider using 'policyHashCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyHashCondition :: Lens.Lens' PutResourcePolicy (Lude.Maybe Lude.Text)
prpPolicyHashCondition = Lens.lens (policyHashCondition :: PutResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyHashCondition = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicyHashCondition "Use generic-lens or generic-optics with 'policyHashCondition' instead." #-}

-- | The ARN of the AWS Glue resource for the resource policy to be set. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpResourceARN :: Lens.Lens' PutResourcePolicy (Lude.Maybe Lude.Text)
prpResourceARN = Lens.lens (resourceARN :: PutResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: PutResourcePolicy)
{-# DEPRECATED prpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Allows you to specify if you want to use both resource-level and account/catalog-level resource policies. A resource-level policy is a policy attached to an individual resource such as a database or a table.
--
-- The default value of @NO@ indicates that resource-level policies cannot co-exist with an account-level policy. A value of @YES@ means the use of both resource-level and account/catalog-level resource policies is allowed.
--
-- /Note:/ Consider using 'enableHybrid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpEnableHybrid :: Lens.Lens' PutResourcePolicy (Lude.Maybe EnableHybridValues)
prpEnableHybrid = Lens.lens (enableHybrid :: PutResourcePolicy -> Lude.Maybe EnableHybridValues) (\s a -> s {enableHybrid = a} :: PutResourcePolicy)
{-# DEPRECATED prpEnableHybrid "Use generic-lens or generic-optics with 'enableHybrid' instead." #-}

-- | Contains the policy document to set, in JSON format.
--
-- /Note:/ Consider using 'policyInJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpPolicyInJSON :: Lens.Lens' PutResourcePolicy Lude.Text
prpPolicyInJSON = Lens.lens (policyInJSON :: PutResourcePolicy -> Lude.Text) (\s a -> s {policyInJSON = a} :: PutResourcePolicy)
{-# DEPRECATED prpPolicyInJSON "Use generic-lens or generic-optics with 'policyInJSON' instead." #-}

instance Lude.AWSRequest PutResourcePolicy where
  type Rs PutResourcePolicy = PutResourcePolicyResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Lude.<$> (x Lude..?> "PolicyHash") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.PutResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyExistsCondition" Lude..=) Lude.<$> policyExistsCondition,
            ("PolicyHashCondition" Lude..=) Lude.<$> policyHashCondition,
            ("ResourceArn" Lude..=) Lude.<$> resourceARN,
            ("EnableHybrid" Lude..=) Lude.<$> enableHybrid,
            Lude.Just ("PolicyInJson" Lude..= policyInJSON)
          ]
      )

instance Lude.ToPath PutResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { policyHash ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyHash' - A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
-- * 'responseStatus' - The response status code.
mkPutResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutResourcePolicyResponse
mkPutResourcePolicyResponse pResponseStatus_ =
  PutResourcePolicyResponse'
    { policyHash = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A hash of the policy that has just been set. This must be included in a subsequent call that overwrites or updates this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsPolicyHash :: Lens.Lens' PutResourcePolicyResponse (Lude.Maybe Lude.Text)
prprsPolicyHash = Lens.lens (policyHash :: PutResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyHash = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsPolicyHash "Use generic-lens or generic-optics with 'policyHash' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prprsResponseStatus :: Lens.Lens' PutResourcePolicyResponse Lude.Int
prprsResponseStatus = Lens.lens (responseStatus :: PutResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutResourcePolicyResponse)
{-# DEPRECATED prprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
