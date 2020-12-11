{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from an object.
module Network.AWS.CloudDirectory.DetachPolicy
  ( -- * Creating a request
    DetachPolicy (..),
    mkDetachPolicy,

    -- ** Request lenses
    dpDirectoryARN,
    dpPolicyReference,
    dpObjectReference,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { directoryARN :: Lude.Text,
    policyReference :: ObjectReference,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
-- * 'objectReference' - Reference that identifies the object whose policy object will be detached.
-- * 'policyReference' - Reference that identifies the policy object.
mkDetachPolicy ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  DetachPolicy
mkDetachPolicy pDirectoryARN_ pPolicyReference_ pObjectReference_ =
  DetachPolicy'
    { directoryARN = pDirectoryARN_,
      policyReference = pPolicyReference_,
      objectReference = pObjectReference_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDirectoryARN :: Lens.Lens' DetachPolicy Lude.Text
dpDirectoryARN = Lens.lens (directoryARN :: DetachPolicy -> Lude.Text) (\s a -> s {directoryARN = a} :: DetachPolicy)
{-# DEPRECATED dpDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyReference :: Lens.Lens' DetachPolicy ObjectReference
dpPolicyReference = Lens.lens (policyReference :: DetachPolicy -> ObjectReference) (\s a -> s {policyReference = a} :: DetachPolicy)
{-# DEPRECATED dpPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | Reference that identifies the object whose policy object will be detached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpObjectReference :: Lens.Lens' DetachPolicy ObjectReference
dpObjectReference = Lens.lens (objectReference :: DetachPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: DetachPolicy)
{-# DEPRECATED dpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DetachPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachPolicy where
  toHeaders DetachPolicy' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyReference" Lude..= policyReference),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath DetachPolicy where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/policy/detach"

instance Lude.ToQuery DetachPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
newtype DetachPolicyResponse = DetachPolicyResponse'
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

-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDetachPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachPolicyResponse
mkDetachPolicyResponse pResponseStatus_ =
  DetachPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DetachPolicyResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DetachPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachPolicyResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
