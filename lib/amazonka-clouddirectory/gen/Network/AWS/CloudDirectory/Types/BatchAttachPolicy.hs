{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchAttachPolicy
  ( BatchAttachPolicy (..)
  -- * Smart constructor
  , mkBatchAttachPolicy
  -- * Lenses
  , bapPolicyReference
  , bapObjectReference
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attaches a policy object to a regular object inside a 'BatchRead' operation. For more information, see 'AttachPolicy' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachPolicy' smart constructor.
data BatchAttachPolicy = BatchAttachPolicy'
  { policyReference :: Types.ObjectReference
    -- ^ The reference that is associated with the policy object.
  , objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object to which the policy will be attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachPolicy' value with any optional fields omitted.
mkBatchAttachPolicy
    :: Types.ObjectReference -- ^ 'policyReference'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> BatchAttachPolicy
mkBatchAttachPolicy policyReference objectReference
  = BatchAttachPolicy'{policyReference, objectReference}

-- | The reference that is associated with the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapPolicyReference :: Lens.Lens' BatchAttachPolicy Types.ObjectReference
bapPolicyReference = Lens.field @"policyReference"
{-# INLINEABLE bapPolicyReference #-}
{-# DEPRECATED policyReference "Use generic-lens or generic-optics with 'policyReference' instead"  #-}

-- | The reference that identifies the object to which the policy will be attached.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bapObjectReference :: Lens.Lens' BatchAttachPolicy Types.ObjectReference
bapObjectReference = Lens.field @"objectReference"
{-# INLINEABLE bapObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

instance Core.FromJSON BatchAttachPolicy where
        toJSON BatchAttachPolicy{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyReference" Core..= policyReference),
                  Core.Just ("ObjectReference" Core..= objectReference)])
