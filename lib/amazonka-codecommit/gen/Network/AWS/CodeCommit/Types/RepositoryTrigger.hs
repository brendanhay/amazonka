{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.RepositoryTrigger
  ( RepositoryTrigger (..)
  -- * Smart constructor
  , mkRepositoryTrigger
  -- * Lenses
  , rtName
  , rtDestinationArn
  , rtEvents
  , rtBranches
  , rtCustomData
  ) where

import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.CodeCommit.Types.BranchName as Types
import qualified Network.AWS.CodeCommit.Types.CustomData as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum as Types
import qualified Network.AWS.CodeCommit.Types.RepositoryTriggerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a trigger for a repository.
--
-- /See:/ 'mkRepositoryTrigger' smart constructor.
data RepositoryTrigger = RepositoryTrigger'
  { name :: Types.RepositoryTriggerName
    -- ^ The name of the trigger.
  , destinationArn :: Types.Arn
    -- ^ The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
  , events :: [Types.RepositoryTriggerEventEnum]
    -- ^ The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS. 
  , branches :: Core.Maybe [Types.BranchName]
    -- ^ The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
  , customData :: Core.Maybe Types.CustomData
    -- ^ Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RepositoryTrigger' value with any optional fields omitted.
mkRepositoryTrigger
    :: Types.RepositoryTriggerName -- ^ 'name'
    -> Types.Arn -- ^ 'destinationArn'
    -> RepositoryTrigger
mkRepositoryTrigger name destinationArn
  = RepositoryTrigger'{name, destinationArn, events = Core.mempty,
                       branches = Core.Nothing, customData = Core.Nothing}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtName :: Lens.Lens' RepositoryTrigger Types.RepositoryTriggerName
rtName = Lens.field @"name"
{-# INLINEABLE rtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ARN of the resource that is the target for a trigger (for example, the ARN of a topic in Amazon SNS).
--
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDestinationArn :: Lens.Lens' RepositoryTrigger Types.Arn
rtDestinationArn = Lens.field @"destinationArn"
{-# INLINEABLE rtDestinationArn #-}
{-# DEPRECATED destinationArn "Use generic-lens or generic-optics with 'destinationArn' instead"  #-}

-- | The repository events that cause the trigger to run actions in another service, such as sending a notification through Amazon SNS. 
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtEvents :: Lens.Lens' RepositoryTrigger [Types.RepositoryTriggerEventEnum]
rtEvents = Lens.field @"events"
{-# INLINEABLE rtEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | The branches to be included in the trigger configuration. If you specify an empty array, the trigger applies to all branches.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtBranches :: Lens.Lens' RepositoryTrigger (Core.Maybe [Types.BranchName])
rtBranches = Lens.field @"branches"
{-# INLINEABLE rtBranches #-}
{-# DEPRECATED branches "Use generic-lens or generic-optics with 'branches' instead"  #-}

-- | Any custom data associated with the trigger to be included in the information sent to the target of the trigger.
--
-- /Note:/ Consider using 'customData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtCustomData :: Lens.Lens' RepositoryTrigger (Core.Maybe Types.CustomData)
rtCustomData = Lens.field @"customData"
{-# INLINEABLE rtCustomData #-}
{-# DEPRECATED customData "Use generic-lens or generic-optics with 'customData' instead"  #-}

instance Core.FromJSON RepositoryTrigger where
        toJSON RepositoryTrigger{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("destinationArn" Core..= destinationArn),
                  Core.Just ("events" Core..= events),
                  ("branches" Core..=) Core.<$> branches,
                  ("customData" Core..=) Core.<$> customData])

instance Core.FromJSON RepositoryTrigger where
        parseJSON
          = Core.withObject "RepositoryTrigger" Core.$
              \ x ->
                RepositoryTrigger' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "destinationArn" Core.<*>
                    x Core..:? "events" Core..!= Core.mempty
                    Core.<*> x Core..:? "branches"
                    Core.<*> x Core..:? "customData"
