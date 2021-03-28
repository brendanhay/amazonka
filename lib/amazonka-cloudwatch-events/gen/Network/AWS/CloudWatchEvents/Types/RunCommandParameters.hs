{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.RunCommandParameters
  ( RunCommandParameters (..)
  -- * Smart constructor
  , mkRunCommandParameters
  -- * Lenses
  , rcpRunCommandTargets
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.RunCommandTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command. 
--
-- /See:/ 'mkRunCommandParameters' smart constructor.
newtype RunCommandParameters = RunCommandParameters'
  { runCommandTargets :: Core.NonEmpty Types.RunCommandTarget
    -- ^ Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RunCommandParameters' value with any optional fields omitted.
mkRunCommandParameters
    :: Core.NonEmpty Types.RunCommandTarget -- ^ 'runCommandTargets'
    -> RunCommandParameters
mkRunCommandParameters runCommandTargets
  = RunCommandParameters'{runCommandTargets}

-- | Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
--
-- /Note:/ Consider using 'runCommandTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpRunCommandTargets :: Lens.Lens' RunCommandParameters (Core.NonEmpty Types.RunCommandTarget)
rcpRunCommandTargets = Lens.field @"runCommandTargets"
{-# INLINEABLE rcpRunCommandTargets #-}
{-# DEPRECATED runCommandTargets "Use generic-lens or generic-optics with 'runCommandTargets' instead"  #-}

instance Core.FromJSON RunCommandParameters where
        toJSON RunCommandParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RunCommandTargets" Core..= runCommandTargets)])

instance Core.FromJSON RunCommandParameters where
        parseJSON
          = Core.withObject "RunCommandParameters" Core.$
              \ x ->
                RunCommandParameters' Core.<$> (x Core..: "RunCommandTargets")
