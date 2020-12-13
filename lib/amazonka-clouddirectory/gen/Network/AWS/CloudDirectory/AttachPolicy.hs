{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy object to a regular object. An object can have a limited number of attached policies.
module Network.AWS.CloudDirectory.AttachPolicy
  ( -- * Creating a request
    AttachPolicy (..),
    mkAttachPolicy,

    -- ** Request lenses
    apDirectoryARN,
    apPolicyReference,
    apObjectReference,

    -- * Destructuring the response
    AttachPolicyResponse (..),
    mkAttachPolicyResponse,

    -- ** Response lenses
    aprsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | The reference that is associated with the policy object.
    policyReference :: ObjectReference,
    -- | The reference that identifies the object to which the policy will be attached.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
-- * 'policyReference' - The reference that is associated with the policy object.
-- * 'objectReference' - The reference that identifies the object to which the policy will be attached.
mkAttachPolicy ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  AttachPolicy
mkAttachPolicy pDirectoryARN_ pPolicyReference_ pObjectReference_ =
  AttachPolicy'
    { directoryARN = pDirectoryARN_,
      policyReference = pPolicyReference_,
      objectReference = pObjectReference_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apDirectoryARN :: Lens.Lens' AttachPolicy Lude.Text
apDirectoryARN = Lens.lens (directoryARN :: AttachPolicy -> Lude.Text) (\s a -> s {directoryARN = a} :: AttachPolicy)
{-# DEPRECATED apDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The reference that is associated with the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyReference :: Lens.Lens' AttachPolicy ObjectReference
apPolicyReference = Lens.lens (policyReference :: AttachPolicy -> ObjectReference) (\s a -> s {policyReference = a} :: AttachPolicy)
{-# DEPRECATED apPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | The reference that identifies the object to which the policy will be attached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apObjectReference :: Lens.Lens' AttachPolicy ObjectReference
apObjectReference = Lens.lens (objectReference :: AttachPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: AttachPolicy)
{-# DEPRECATED apObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest AttachPolicy where
  type Rs AttachPolicy = AttachPolicyResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AttachPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachPolicy where
  toHeaders AttachPolicy' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyReference" Lude..= policyReference),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath AttachPolicy where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/policy/attach"

instance Lude.ToQuery AttachPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachPolicyResponse' smart constructor.
newtype AttachPolicyResponse = AttachPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAttachPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachPolicyResponse
mkAttachPolicyResponse pResponseStatus_ =
  AttachPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsResponseStatus :: Lens.Lens' AttachPolicyResponse Lude.Int
aprsResponseStatus = Lens.lens (responseStatus :: AttachPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachPolicyResponse)
{-# DEPRECATED aprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
