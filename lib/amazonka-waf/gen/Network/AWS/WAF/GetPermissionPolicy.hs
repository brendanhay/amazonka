{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetPermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM policy attached to the RuleGroup.
module Network.AWS.WAF.GetPermissionPolicy
  ( -- * Creating a request
    GetPermissionPolicy (..),
    mkGetPermissionPolicy,

    -- ** Request lenses
    gppResourceARN,

    -- * Destructuring the response
    GetPermissionPolicyResponse (..),
    mkGetPermissionPolicyResponse,

    -- ** Response lenses
    gpprsPolicy,
    gpprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetPermissionPolicy' smart constructor.
newtype GetPermissionPolicy = GetPermissionPolicy'
  { -- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPermissionPolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
mkGetPermissionPolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  GetPermissionPolicy
mkGetPermissionPolicy pResourceARN_ =
  GetPermissionPolicy' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppResourceARN :: Lens.Lens' GetPermissionPolicy Lude.Text
gppResourceARN = Lens.lens (resourceARN :: GetPermissionPolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: GetPermissionPolicy)
{-# DEPRECATED gppResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetPermissionPolicy where
  type Rs GetPermissionPolicy = GetPermissionPolicyResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPermissionPolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPermissionPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetPermissionPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPermissionPolicy where
  toJSON GetPermissionPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetPermissionPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPermissionPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPermissionPolicyResponse' smart constructor.
data GetPermissionPolicyResponse = GetPermissionPolicyResponse'
  { -- | The IAM policy attached to the specified RuleGroup.
    policy :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPermissionPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The IAM policy attached to the specified RuleGroup.
-- * 'responseStatus' - The response status code.
mkGetPermissionPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPermissionPolicyResponse
mkGetPermissionPolicyResponse pResponseStatus_ =
  GetPermissionPolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The IAM policy attached to the specified RuleGroup.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpprsPolicy :: Lens.Lens' GetPermissionPolicyResponse (Lude.Maybe Lude.Text)
gpprsPolicy = Lens.lens (policy :: GetPermissionPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetPermissionPolicyResponse)
{-# DEPRECATED gpprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpprsResponseStatus :: Lens.Lens' GetPermissionPolicyResponse Lude.Int
gpprsResponseStatus = Lens.lens (responseStatus :: GetPermissionPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPermissionPolicyResponse)
{-# DEPRECATED gpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
