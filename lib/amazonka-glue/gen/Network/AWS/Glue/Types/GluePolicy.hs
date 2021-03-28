{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GluePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.GluePolicy
  ( GluePolicy (..)
  -- * Smart constructor
  , mkGluePolicy
  -- * Lenses
  , gpCreateTime
  , gpPolicyHash
  , gpPolicyInJson
  , gpUpdateTime
  ) where

import qualified Network.AWS.Glue.Types.HashString as Types
import qualified Network.AWS.Glue.Types.PolicyJsonString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure for returning a resource policy.
--
-- /See:/ 'mkGluePolicy' smart constructor.
data GluePolicy = GluePolicy'
  { createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the policy was created.
  , policyHash :: Core.Maybe Types.HashString
    -- ^ Contains the hash value associated with this policy.
  , policyInJson :: Core.Maybe Types.PolicyJsonString
    -- ^ Contains the requested policy document, in JSON format.
  , updateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time at which the policy was last updated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GluePolicy' value with any optional fields omitted.
mkGluePolicy
    :: GluePolicy
mkGluePolicy
  = GluePolicy'{createTime = Core.Nothing, policyHash = Core.Nothing,
                policyInJson = Core.Nothing, updateTime = Core.Nothing}

-- | The date and time at which the policy was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpCreateTime :: Lens.Lens' GluePolicy (Core.Maybe Core.NominalDiffTime)
gpCreateTime = Lens.field @"createTime"
{-# INLINEABLE gpCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | Contains the hash value associated with this policy.
--
-- /Note:/ Consider using 'policyHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyHash :: Lens.Lens' GluePolicy (Core.Maybe Types.HashString)
gpPolicyHash = Lens.field @"policyHash"
{-# INLINEABLE gpPolicyHash #-}
{-# DEPRECATED policyHash "Use generic-lens or generic-optics with 'policyHash' instead"  #-}

-- | Contains the requested policy document, in JSON format.
--
-- /Note:/ Consider using 'policyInJson' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpPolicyInJson :: Lens.Lens' GluePolicy (Core.Maybe Types.PolicyJsonString)
gpPolicyInJson = Lens.field @"policyInJson"
{-# INLINEABLE gpPolicyInJson #-}
{-# DEPRECATED policyInJson "Use generic-lens or generic-optics with 'policyInJson' instead"  #-}

-- | The date and time at which the policy was last updated.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpUpdateTime :: Lens.Lens' GluePolicy (Core.Maybe Core.NominalDiffTime)
gpUpdateTime = Lens.field @"updateTime"
{-# INLINEABLE gpUpdateTime #-}
{-# DEPRECATED updateTime "Use generic-lens or generic-optics with 'updateTime' instead"  #-}

instance Core.FromJSON GluePolicy where
        parseJSON
          = Core.withObject "GluePolicy" Core.$
              \ x ->
                GluePolicy' Core.<$>
                  (x Core..:? "CreateTime") Core.<*> x Core..:? "PolicyHash" Core.<*>
                    x Core..:? "PolicyInJson"
                    Core.<*> x Core..:? "UpdateTime"
