{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html resource-based IAM policy> for a function, version, or alias.
module Network.AWS.Lambda.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpQualifier,
    gpFunctionName,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprsPolicy,
    gprsRevisionId,
    gprsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { qualifier :: Lude.Maybe Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- * 'functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
-- * 'qualifier' - Specify a version or alias to get the policy for that resource.
mkGetPolicy ::
  -- | 'functionName'
  Lude.Text ->
  GetPolicy
mkGetPolicy pFunctionName_ =
  GetPolicy'
    { qualifier = Lude.Nothing,
      functionName = pFunctionName_
    }

-- | Specify a version or alias to get the policy for that resource.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpQualifier :: Lens.Lens' GetPolicy (Lude.Maybe Lude.Text)
gpQualifier = Lens.lens (qualifier :: GetPolicy -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: GetPolicy)
{-# DEPRECATED gpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpFunctionName :: Lens.Lens' GetPolicy Lude.Text
gpFunctionName = Lens.lens (functionName :: GetPolicy -> Lude.Text) (\s a -> s {functionName = a} :: GetPolicy)
{-# DEPRECATED gpFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Lude.<$> (x Lude..?> "Policy")
            Lude.<*> (x Lude..?> "RevisionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/policy"]

instance Lude.ToQuery GetPolicy where
  toQuery GetPolicy' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { policy ::
      Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The resource-based policy.
-- * 'responseStatus' - The response status code.
-- * 'revisionId' - A unique identifier for the current revision of the policy.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyResponse
mkGetPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { policy = Lude.Nothing,
      revisionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource-based policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicy :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicy = Lens.lens (policy :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A unique identifier for the current revision of the policy.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsRevisionId :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsRevisionId = Lens.lens (revisionId :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: GetPolicyResponse)
{-# DEPRECATED gprsRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPolicyResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
