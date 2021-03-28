{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PolicyAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.PolicyAttachment
  ( PolicyAttachment (..)
  -- * Smart constructor
  , mkPolicyAttachment
  -- * Lenses
  , paObjectIdentifier
  , paPolicyId
  , paPolicyType
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.CloudDirectory.Types.PolicyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the @PolicyType@ , @PolicyId@ , and the @ObjectIdentifier@ to which it is attached. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
-- /See:/ 'mkPolicyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
  , policyId :: Core.Maybe Types.ObjectIdentifier
    -- ^ The ID of @PolicyAttachment@ .
  , policyType :: Core.Maybe Types.PolicyType
    -- ^ The type of policy that can be associated with @PolicyAttachment@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyAttachment' value with any optional fields omitted.
mkPolicyAttachment
    :: PolicyAttachment
mkPolicyAttachment
  = PolicyAttachment'{objectIdentifier = Core.Nothing,
                      policyId = Core.Nothing, policyType = Core.Nothing}

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paObjectIdentifier :: Lens.Lens' PolicyAttachment (Core.Maybe Types.ObjectIdentifier)
paObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE paObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The ID of @PolicyAttachment@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPolicyId :: Lens.Lens' PolicyAttachment (Core.Maybe Types.ObjectIdentifier)
paPolicyId = Lens.field @"policyId"
{-# INLINEABLE paPolicyId #-}
{-# DEPRECATED policyId "Use generic-lens or generic-optics with 'policyId' instead"  #-}

-- | The type of policy that can be associated with @PolicyAttachment@ .
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPolicyType :: Lens.Lens' PolicyAttachment (Core.Maybe Types.PolicyType)
paPolicyType = Lens.field @"policyType"
{-# INLINEABLE paPolicyType #-}
{-# DEPRECATED policyType "Use generic-lens or generic-optics with 'policyType' instead"  #-}

instance Core.FromJSON PolicyAttachment where
        parseJSON
          = Core.withObject "PolicyAttachment" Core.$
              \ x ->
                PolicyAttachment' Core.<$>
                  (x Core..:? "ObjectIdentifier") Core.<*> x Core..:? "PolicyId"
                    Core.<*> x Core..:? "PolicyType"
