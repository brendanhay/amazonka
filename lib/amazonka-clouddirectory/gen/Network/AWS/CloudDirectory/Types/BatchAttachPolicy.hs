{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachPolicy
  ( BatchAttachPolicy (..),

    -- * Smart constructor
    mkBatchAttachPolicy,

    -- * Lenses
    bapPolicyReference,
    bapObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attaches a policy object to a regular object inside a 'BatchRead' operation. For more information, see 'AttachPolicy' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachPolicy' smart constructor.
data BatchAttachPolicy = BatchAttachPolicy'
  { -- | The reference that is associated with the policy object.
    policyReference :: ObjectReference,
    -- | The reference that identifies the object to which the policy will be attached.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachPolicy' with the minimum fields required to make a request.
--
-- * 'policyReference' - The reference that is associated with the policy object.
-- * 'objectReference' - The reference that identifies the object to which the policy will be attached.
mkBatchAttachPolicy ::
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  BatchAttachPolicy
mkBatchAttachPolicy pPolicyReference_ pObjectReference_ =
  BatchAttachPolicy'
    { policyReference = pPolicyReference_,
      objectReference = pObjectReference_
    }

-- | The reference that is associated with the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapPolicyReference :: Lens.Lens' BatchAttachPolicy ObjectReference
bapPolicyReference = Lens.lens (policyReference :: BatchAttachPolicy -> ObjectReference) (\s a -> s {policyReference = a} :: BatchAttachPolicy)
{-# DEPRECATED bapPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | The reference that identifies the object to which the policy will be attached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapObjectReference :: Lens.Lens' BatchAttachPolicy ObjectReference
bapObjectReference = Lens.lens (objectReference :: BatchAttachPolicy -> ObjectReference) (\s a -> s {objectReference = a} :: BatchAttachPolicy)
{-# DEPRECATED bapObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchAttachPolicy where
  toJSON BatchAttachPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyReference" Lude..= policyReference),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
