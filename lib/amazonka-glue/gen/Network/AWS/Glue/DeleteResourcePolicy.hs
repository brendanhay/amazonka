{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified policy.
module Network.AWS.Glue.DeleteResourcePolicy
  ( -- * Creating a request
    DeleteResourcePolicy (..),
    mkDeleteResourcePolicy,

    -- ** Request lenses
    drpPolicyHashCondition,
    drpResourceARN,

    -- * Destructuring the response
    DeleteResourcePolicyResponse (..),
    mkDeleteResourcePolicyResponse,

    -- ** Response lenses
    drprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | The hash value returned when this policy was set.
    policyHashCondition :: Lude.Maybe Lude.Text,
    -- | The ARN of the AWS Glue resource for the resource policy to be deleted.
    resourceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicy' with the minimum fields required to make a request.
--
-- * 'policyHashCondition' - The hash value returned when this policy was set.
-- * 'resourceARN' - The ARN of the AWS Glue resource for the resource policy to be deleted.
mkDeleteResourcePolicy ::
  DeleteResourcePolicy
mkDeleteResourcePolicy =
  DeleteResourcePolicy'
    { policyHashCondition = Lude.Nothing,
      resourceARN = Lude.Nothing
    }

-- | The hash value returned when this policy was set.
--
-- /Note:/ Consider using 'policyHashCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpPolicyHashCondition :: Lens.Lens' DeleteResourcePolicy (Lude.Maybe Lude.Text)
drpPolicyHashCondition = Lens.lens (policyHashCondition :: DeleteResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {policyHashCondition = a} :: DeleteResourcePolicy)
{-# DEPRECATED drpPolicyHashCondition "Use generic-lens or generic-optics with 'policyHashCondition' instead." #-}

-- | The ARN of the AWS Glue resource for the resource policy to be deleted.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpResourceARN :: Lens.Lens' DeleteResourcePolicy (Lude.Maybe Lude.Text)
drpResourceARN = Lens.lens (resourceARN :: DeleteResourcePolicy -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: DeleteResourcePolicy)
{-# DEPRECATED drpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteResourcePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyHashCondition" Lude..=) Lude.<$> policyHashCondition,
            ("ResourceArn" Lude..=) Lude.<$> resourceARN
          ]
      )

instance Lude.ToPath DeleteResourcePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteResourcePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
newtype DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteResourcePolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteResourcePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse pResponseStatus_ =
  DeleteResourcePolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprsResponseStatus :: Lens.Lens' DeleteResourcePolicyResponse Lude.Int
drprsResponseStatus = Lens.lens (responseStatus :: DeleteResourcePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteResourcePolicyResponse)
{-# DEPRECATED drprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
