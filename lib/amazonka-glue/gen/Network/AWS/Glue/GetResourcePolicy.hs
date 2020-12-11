{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified resource policy.
module Network.AWS.Glue.GetResourcePolicy
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpResourceARN,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprrsPolicyInJSON,
    grprrsUpdateTime,
    grprrsPolicyHash,
    grprrsCreateTime,
    grprrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { resourceARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
mkGetResourcePolicy ::
  GetResourcePolicy
mkGetResourcePolicy =
  GetResourcePolicy' {resourceARN = Lude.Nothing}

-- | The ARN of the AWS Glue resource for the resource policy to be retrieved. For more information about AWS Glue resource ARNs, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceARN :: Lens.Lens' GetResourcePolicy (Lude.Maybe Lude.Text)
grpResourceARN = Lens.lens (resourceARN :: GetResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: GetResourcePolicy)
{-# DEPRECATED grpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Lude.<$> (x Lude..?> "PolicyInJson")
            Lude.<*> (x Lude..?> "UpdateTime")
            Lude.<*> (x Lude..?> "PolicyHash")
            Lude.<*> (x Lude..?> "CreateTime")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Lude.object
      (Lude.catMaybes [("ResourceArn" Lude..=) Lude.<$> resourceARN])

instance Lude.ToPath GetResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { policyInJSON ::
      Lude.Maybe Lude.Text,
    updateTime :: Lude.Maybe Lude.Timestamp,
    policyHash :: Lude.Maybe Lude.Text,
    createTime :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'createTime' - The date and time at which the policy was created.
-- * 'policyHash' - Contains the hash value associated with this policy.
-- * 'policyInJSON' - Contains the requested policy document, in JSON format.
-- * 'responseStatus' - The response status code.
-- * 'updateTime' - The date and time at which the policy was last updated.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { policyInJSON = Lude.Nothing,
      updateTime = Lude.Nothing,
      policyHash = Lude.Nothing,
      createTime = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the requested policy document, in JSON format.
--
-- /Note:/ Consider using 'policyInJSON' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyInJSON :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprrsPolicyInJSON = Lens.lens (policyInJSON :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyInJSON = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprrsPolicyInJSON "Use generic-lens or generic-optics with 'policyInJSON' instead." #-}

-- | The date and time at which the policy was last updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsUpdateTime :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Timestamp)
grprrsUpdateTime = Lens.lens (updateTime :: GetResourcePolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateTime = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprrsUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | Contains the hash value associated with this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicyHash :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprrsPolicyHash = Lens.lens (policyHash :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyHash = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprrsPolicyHash "Use generic-lens or generic-optics with 'policyHash' instead." #-}

-- | The date and time at which the policy was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsCreateTime :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Timestamp)
grprrsCreateTime = Lens.lens (createTime :: GetResourcePolicyResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprrsCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Lude.Int
grprrsResponseStatus = Lens.lens (responseStatus :: GetResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
