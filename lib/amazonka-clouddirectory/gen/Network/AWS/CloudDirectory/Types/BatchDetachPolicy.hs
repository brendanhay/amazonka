{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachPolicy
  ( BatchDetachPolicy (..),

    -- * Smart constructor
    mkBatchDetachPolicy,

    -- * Lenses
    bdpPolicyReference,
    bdpObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detaches the specified policy from the specified directory inside a 'BatchWrite' operation. For more information, see 'DetachPolicy' and 'BatchWriteRequest$Operations' .
--
-- /See:/ 'mkBatchDetachPolicy' smart constructor.
data BatchDetachPolicy = BatchDetachPolicy'
  { -- | Reference that identifies the policy object.
    policyReference :: ObjectReference,
    -- | Reference that identifies the object whose policy object will be detached.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetachPolicy' with the minimum fields required to make a request.
--
-- * 'policyReference' - Reference that identifies the policy object.
-- * 'objectReference' - Reference that identifies the object whose policy object will be detached.
mkBatchDetachPolicy ::
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  BatchDetachPolicy
mkBatchDetachPolicy pPolicyReference_ pObjectReference_ =
  BatchDetachPolicy'
    { policyReference = pPolicyReference_,
      objectReference = pObjectReference_
    }

-- | Reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpPolicyReference :: Lens.Lens' BatchDetachPolicy ObjectReference
bdpPolicyReference = Lens.lens (policyReference :: BatchDetachPolicy -> ObjectReference) (\s a -> s {policyReference = a} :: BatchDetachPolicy)
{-# DEPRECATED bdpPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | Reference that identifies the object whose policy object will be detached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpObjectReference :: Lens.Lens' BatchDetachPolicy ObjectReference
bdpObjectReference = Lens.lens (objectReference :: BatchDetachPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: BatchDetachPolicy)
{-# DEPRECATED bdpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchDetachPolicy where
  toJSON BatchDetachPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyReference" Lude..= policyReference),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
