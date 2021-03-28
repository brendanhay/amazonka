{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.PolicyVersion
  ( PolicyVersion (..)
  -- * Smart constructor
  , mkPolicyVersion
  -- * Lenses
  , pvCreateDate
  , pvIsDefaultVersion
  , pvVersionId
  ) where

import qualified Network.AWS.IoT.Types.VersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a policy version.
--
-- /See:/ 'mkPolicyVersion' smart constructor.
data PolicyVersion = PolicyVersion'
  { createDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the policy was created.
  , isDefaultVersion :: Core.Maybe Core.Bool
    -- ^ Specifies whether the policy version is the default.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ The policy version ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PolicyVersion' value with any optional fields omitted.
mkPolicyVersion
    :: PolicyVersion
mkPolicyVersion
  = PolicyVersion'{createDate = Core.Nothing,
                   isDefaultVersion = Core.Nothing, versionId = Core.Nothing}

-- | The date and time the policy was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCreateDate :: Lens.Lens' PolicyVersion (Core.Maybe Core.NominalDiffTime)
pvCreateDate = Lens.field @"createDate"
{-# INLINEABLE pvCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | Specifies whether the policy version is the default.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvIsDefaultVersion :: Lens.Lens' PolicyVersion (Core.Maybe Core.Bool)
pvIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# INLINEABLE pvIsDefaultVersion #-}
{-# DEPRECATED isDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead"  #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVersionId :: Lens.Lens' PolicyVersion (Core.Maybe Types.VersionId)
pvVersionId = Lens.field @"versionId"
{-# INLINEABLE pvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromJSON PolicyVersion where
        parseJSON
          = Core.withObject "PolicyVersion" Core.$
              \ x ->
                PolicyVersion' Core.<$>
                  (x Core..:? "createDate") Core.<*> x Core..:? "isDefaultVersion"
                    Core.<*> x Core..:? "versionId"
