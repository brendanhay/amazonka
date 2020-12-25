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

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detaches the specified policy from the specified directory inside a 'BatchWrite' operation. For more information, see 'DetachPolicy' and 'BatchWriteRequest$Operations' .
--
-- /See:/ 'mkBatchDetachPolicy' smart constructor.
data BatchDetachPolicy = BatchDetachPolicy'
  { -- | Reference that identifies the policy object.
    policyReference :: Types.ObjectReference,
    -- | Reference that identifies the object whose policy object will be detached.
    objectReference :: Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachPolicy' value with any optional fields omitted.
mkBatchDetachPolicy ::
  -- | 'policyReference'
  Types.ObjectReference ->
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchDetachPolicy
mkBatchDetachPolicy policyReference objectReference =
  BatchDetachPolicy' {policyReference, objectReference}

-- | Reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpPolicyReference :: Lens.Lens' BatchDetachPolicy Types.ObjectReference
bdpPolicyReference = Lens.field @"policyReference"
{-# DEPRECATED bdpPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | Reference that identifies the object whose policy object will be detached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdpObjectReference :: Lens.Lens' BatchDetachPolicy Types.ObjectReference
bdpObjectReference = Lens.field @"objectReference"
{-# DEPRECATED bdpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Core.FromJSON BatchDetachPolicy where
  toJSON BatchDetachPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyReference" Core..= policyReference),
            Core.Just ("ObjectReference" Core..= objectReference)
          ]
      )
