{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.PutPermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an IAM policy to the specified resource. The only supported use for this action is to share a RuleGroup across accounts.
--
-- The @PutPermissionPolicy@ is subject to the following restrictions:
--
--     * You can attach only one policy with each @PutPermissionPolicy@ request.
--
--
--     * The policy must include an @Effect@ , @Action@ and @Principal@ .
--
--
--     * @Effect@ must specify @Allow@ .
--
--
--     * The @Action@ in the policy must be @waf:UpdateWebACL@ , @waf-regional:UpdateWebACL@ , @waf:GetRuleGroup@ and @waf-regional:GetRuleGroup@ . Any extra or wildcard actions in the policy will be rejected.
--
--
--     * The policy cannot include a @Resource@ parameter.
--
--
--     * The ARN in the request must be a valid WAF RuleGroup ARN and the RuleGroup must exist in the same region.
--
--
--     * The user making the request must be the owner of the RuleGroup.
--
--
--     * Your policy must be composed using IAM Policy version 2012-10-17.
--
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> .
-- An example of a valid policy parameter is shown in the Examples section below.
module Network.AWS.WAF.PutPermissionPolicy
  ( -- * Creating a request
    PutPermissionPolicy (..),
    mkPutPermissionPolicy,

    -- ** Request lenses
    pppResourceARN,
    pppPolicy,

    -- * Destructuring the response
    PutPermissionPolicyResponse (..),
    mkPutPermissionPolicyResponse,

    -- ** Response lenses
    ppprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkPutPermissionPolicy' smart constructor.
data PutPermissionPolicy = PutPermissionPolicy'
  { resourceARN ::
      Lude.Text,
    policy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPermissionPolicy' with the minimum fields required to make a request.
--
-- * 'policy' - The policy to attach to the specified RuleGroup.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
mkPutPermissionPolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutPermissionPolicy
mkPutPermissionPolicy pResourceARN_ pPolicy_ =
  PutPermissionPolicy'
    { resourceARN = pResourceARN_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the RuleGroup to which you want to attach the policy.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppResourceARN :: Lens.Lens' PutPermissionPolicy Lude.Text
pppResourceARN = Lens.lens (resourceARN :: PutPermissionPolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: PutPermissionPolicy)
{-# DEPRECATED pppResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The policy to attach to the specified RuleGroup.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pppPolicy :: Lens.Lens' PutPermissionPolicy Lude.Text
pppPolicy = Lens.lens (policy :: PutPermissionPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutPermissionPolicy)
{-# DEPRECATED pppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutPermissionPolicy where
  type Rs PutPermissionPolicy = PutPermissionPolicyResponse
  request = Req.postJSON wafService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutPermissionPolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutPermissionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.PutPermissionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPermissionPolicy where
  toJSON PutPermissionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceArn" Lude..= resourceARN),
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutPermissionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPermissionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutPermissionPolicyResponse' smart constructor.
newtype PutPermissionPolicyResponse = PutPermissionPolicyResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPermissionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutPermissionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutPermissionPolicyResponse
mkPutPermissionPolicyResponse pResponseStatus_ =
  PutPermissionPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppprsResponseStatus :: Lens.Lens' PutPermissionPolicyResponse Lude.Int
ppprsResponseStatus = Lens.lens (responseStatus :: PutPermissionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutPermissionPolicyResponse)
{-# DEPRECATED ppprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
