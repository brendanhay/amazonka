{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.InstanceInfo
  ( InstanceInfo (..)
  -- * Smart constructor
  , mkInstanceInfo
  -- * Lenses
  , iiDeregisterTime
  , iiIamSessionArn
  , iiIamUserArn
  , iiInstanceArn
  , iiInstanceName
  , iiRegisterTime
  , iiTags
  ) where

import qualified Network.AWS.CodeDeploy.Types.IamSessionArn as Types
import qualified Network.AWS.CodeDeploy.Types.IamUserArn as Types
import qualified Network.AWS.CodeDeploy.Types.InstanceArn as Types
import qualified Network.AWS.CodeDeploy.Types.InstanceName as Types
import qualified Network.AWS.CodeDeploy.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an on-premises instance.
--
-- /See:/ 'mkInstanceInfo' smart constructor.
data InstanceInfo = InstanceInfo'
  { deregisterTime :: Core.Maybe Core.NominalDiffTime
    -- ^ If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
  , iamSessionArn :: Core.Maybe Types.IamSessionArn
    -- ^ The ARN of the IAM session associated with the on-premises instance.
  , iamUserArn :: Core.Maybe Types.IamUserArn
    -- ^ The IAM user ARN associated with the on-premises instance.
  , instanceArn :: Core.Maybe Types.InstanceArn
    -- ^ The ARN of the on-premises instance.
  , instanceName :: Core.Maybe Types.InstanceName
    -- ^ The name of the on-premises instance.
  , registerTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the on-premises instance was registered.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags currently associated with the on-premises instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceInfo' value with any optional fields omitted.
mkInstanceInfo
    :: InstanceInfo
mkInstanceInfo
  = InstanceInfo'{deregisterTime = Core.Nothing,
                  iamSessionArn = Core.Nothing, iamUserArn = Core.Nothing,
                  instanceArn = Core.Nothing, instanceName = Core.Nothing,
                  registerTime = Core.Nothing, tags = Core.Nothing}

-- | If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
--
-- /Note:/ Consider using 'deregisterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDeregisterTime :: Lens.Lens' InstanceInfo (Core.Maybe Core.NominalDiffTime)
iiDeregisterTime = Lens.field @"deregisterTime"
{-# INLINEABLE iiDeregisterTime #-}
{-# DEPRECATED deregisterTime "Use generic-lens or generic-optics with 'deregisterTime' instead"  #-}

-- | The ARN of the IAM session associated with the on-premises instance.
--
-- /Note:/ Consider using 'iamSessionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIamSessionArn :: Lens.Lens' InstanceInfo (Core.Maybe Types.IamSessionArn)
iiIamSessionArn = Lens.field @"iamSessionArn"
{-# INLINEABLE iiIamSessionArn #-}
{-# DEPRECATED iamSessionArn "Use generic-lens or generic-optics with 'iamSessionArn' instead"  #-}

-- | The IAM user ARN associated with the on-premises instance.
--
-- /Note:/ Consider using 'iamUserArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIamUserArn :: Lens.Lens' InstanceInfo (Core.Maybe Types.IamUserArn)
iiIamUserArn = Lens.field @"iamUserArn"
{-# INLINEABLE iiIamUserArn #-}
{-# DEPRECATED iamUserArn "Use generic-lens or generic-optics with 'iamUserArn' instead"  #-}

-- | The ARN of the on-premises instance.
--
-- /Note:/ Consider using 'instanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceArn :: Lens.Lens' InstanceInfo (Core.Maybe Types.InstanceArn)
iiInstanceArn = Lens.field @"instanceArn"
{-# INLINEABLE iiInstanceArn #-}
{-# DEPRECATED instanceArn "Use generic-lens or generic-optics with 'instanceArn' instead"  #-}

-- | The name of the on-premises instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceName :: Lens.Lens' InstanceInfo (Core.Maybe Types.InstanceName)
iiInstanceName = Lens.field @"instanceName"
{-# INLINEABLE iiInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | The time at which the on-premises instance was registered.
--
-- /Note:/ Consider using 'registerTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiRegisterTime :: Lens.Lens' InstanceInfo (Core.Maybe Core.NominalDiffTime)
iiRegisterTime = Lens.field @"registerTime"
{-# INLINEABLE iiRegisterTime #-}
{-# DEPRECATED registerTime "Use generic-lens or generic-optics with 'registerTime' instead"  #-}

-- | The tags currently associated with the on-premises instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTags :: Lens.Lens' InstanceInfo (Core.Maybe [Types.Tag])
iiTags = Lens.field @"tags"
{-# INLINEABLE iiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON InstanceInfo where
        parseJSON
          = Core.withObject "InstanceInfo" Core.$
              \ x ->
                InstanceInfo' Core.<$>
                  (x Core..:? "deregisterTime") Core.<*> x Core..:? "iamSessionArn"
                    Core.<*> x Core..:? "iamUserArn"
                    Core.<*> x Core..:? "instanceArn"
                    Core.<*> x Core..:? "instanceName"
                    Core.<*> x Core..:? "registerTime"
                    Core.<*> x Core..:? "tags"
