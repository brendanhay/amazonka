{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.GetResourcePolicy
  ( -- * Creating a request
    GetResourcePolicy (..),
    mkGetResourcePolicy,

    -- ** Request lenses
    grpResourceARN,

    -- * Destructuring the response
    GetResourcePolicyResponse (..),
    mkGetResourcePolicyResponse,

    -- ** Response lenses
    grprsPolicy,
    grprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the resource that is associated with the resource policy.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the resource that is associated with the resource policy.
mkGetResourcePolicy ::
  -- | 'resourceARN'
  Lude.Text ->
  GetResourcePolicy
mkGetResourcePolicy pResourceARN_ =
  GetResourcePolicy' {resourceARN = pResourceARN_}

-- | The ARN of the resource that is associated with the resource policy.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceARN :: Lens.Lens' GetResourcePolicy Lude.Text
grpResourceARN = Lens.lens (resourceARN :: GetResourcePolicy -> Lude.Text) (\s a -> s {resourceARN = a} :: GetResourcePolicy)
{-# DEPRECATED grpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Lude.<$> (x Lude..?> "policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.GetResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("resourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery GetResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The resource policy for the resource identified by the input ARN parameter.
    policy :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - The resource policy for the resource identified by the input ARN parameter.
-- * 'responseStatus' - The response status code.
mkGetResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetResourcePolicyResponse
mkGetResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resource policy for the resource identified by the input ARN parameter.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsPolicy :: Lens.Lens' GetResourcePolicyResponse (Lude.Maybe Lude.Text)
grprsPolicy = Lens.lens (policy :: GetResourcePolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Lude.Int
grprsResponseStatus = Lens.lens (responseStatus :: GetResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetResourcePolicyResponse)
{-# DEPRECATED grprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
