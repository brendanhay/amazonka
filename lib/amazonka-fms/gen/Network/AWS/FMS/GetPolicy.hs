{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager policy.
module Network.AWS.FMS.GetPolicy
  ( -- * Creating a request
    GetPolicy (..),
    mkGetPolicy,

    -- ** Request lenses
    gpPolicyId,

    -- * Destructuring the response
    GetPolicyResponse (..),
    mkGetPolicyResponse,

    -- ** Response lenses
    gprsPolicyARN,
    gprsPolicy,
    gprsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    policyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- * 'policyId' - The ID of the AWS Firewall Manager policy that you want the details for.
mkGetPolicy ::
  -- | 'policyId'
  Lude.Text ->
  GetPolicy
mkGetPolicy pPolicyId_ = GetPolicy' {policyId = pPolicyId_}

-- | The ID of the AWS Firewall Manager policy that you want the details for.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyId :: Lens.Lens' GetPolicy Lude.Text
gpPolicyId = Lens.lens (policyId :: GetPolicy -> Lude.Text) (\s a -> s {policyId = a} :: GetPolicy)
{-# DEPRECATED gpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Lude.AWSRequest GetPolicy where
  type Rs GetPolicy = GetPolicyResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Lude.<$> (x Lude..?> "PolicyArn")
            Lude.<*> (x Lude..?> "Policy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PolicyId" Lude..= policyId)])

instance Lude.ToPath GetPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the specified policy.
    policyARN :: Lude.Maybe Lude.Text,
    -- | Information about the specified AWS Firewall Manager policy.
    policy :: Lude.Maybe Policy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the specified policy.
-- * 'policy' - Information about the specified AWS Firewall Manager policy.
-- * 'responseStatus' - The response status code.
mkGetPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetPolicyResponse
mkGetPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { policyARN = Lude.Nothing,
      policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the specified policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicyARN :: Lens.Lens' GetPolicyResponse (Lude.Maybe Lude.Text)
gprsPolicyARN = Lens.lens (policyARN :: GetPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | Information about the specified AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsPolicy :: Lens.Lens' GetPolicyResponse (Lude.Maybe Policy)
gprsPolicy = Lens.lens (policy :: GetPolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: GetPolicyResponse)
{-# DEPRECATED gprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprsResponseStatus :: Lens.Lens' GetPolicyResponse Lude.Int
gprsResponseStatus = Lens.lens (responseStatus :: GetPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetPolicyResponse)
{-# DEPRECATED gprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
