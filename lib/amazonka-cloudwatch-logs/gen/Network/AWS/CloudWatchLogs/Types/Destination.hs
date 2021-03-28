{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.Destination
  ( Destination (..)
  -- * Smart constructor
  , mkDestination
  -- * Lenses
  , dAccessPolicy
  , dArn
  , dCreationTime
  , dDestinationName
  , dRoleArn
  , dTargetArn
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.AccessPolicy as Types
import qualified Network.AWS.CloudWatchLogs.Types.Arn as Types
import qualified Network.AWS.CloudWatchLogs.Types.DestinationName as Types
import qualified Network.AWS.CloudWatchLogs.Types.RoleArn as Types
import qualified Network.AWS.CloudWatchLogs.Types.TargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a cross-account destination that receives subscription log events.
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { accessPolicy :: Core.Maybe Types.AccessPolicy
    -- ^ An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
  , arn :: Core.Maybe Types.Arn
    -- ^ The ARN of this destination.
  , creationTime :: Core.Maybe Core.Natural
    -- ^ The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
  , destinationName :: Core.Maybe Types.DestinationName
    -- ^ The name of the destination.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ A role for impersonation, used when delivering log events to the target.
  , targetArn :: Core.Maybe Types.TargetArn
    -- ^ The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Destination' value with any optional fields omitted.
mkDestination
    :: Destination
mkDestination
  = Destination'{accessPolicy = Core.Nothing, arn = Core.Nothing,
                 creationTime = Core.Nothing, destinationName = Core.Nothing,
                 roleArn = Core.Nothing, targetArn = Core.Nothing}

-- | An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
--
-- /Note:/ Consider using 'accessPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessPolicy :: Lens.Lens' Destination (Core.Maybe Types.AccessPolicy)
dAccessPolicy = Lens.field @"accessPolicy"
{-# INLINEABLE dAccessPolicy #-}
{-# DEPRECATED accessPolicy "Use generic-lens or generic-optics with 'accessPolicy' instead"  #-}

-- | The ARN of this destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Destination (Core.Maybe Types.Arn)
dArn = Lens.field @"arn"
{-# INLINEABLE dArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreationTime :: Lens.Lens' Destination (Core.Maybe Core.Natural)
dCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The name of the destination.
--
-- /Note:/ Consider using 'destinationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationName :: Lens.Lens' Destination (Core.Maybe Types.DestinationName)
dDestinationName = Lens.field @"destinationName"
{-# INLINEABLE dDestinationName #-}
{-# DEPRECATED destinationName "Use generic-lens or generic-optics with 'destinationName' instead"  #-}

-- | A role for impersonation, used when delivering log events to the target.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleArn :: Lens.Lens' Destination (Core.Maybe Types.RoleArn)
dRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTargetArn :: Lens.Lens' Destination (Core.Maybe Types.TargetArn)
dTargetArn = Lens.field @"targetArn"
{-# INLINEABLE dTargetArn #-}
{-# DEPRECATED targetArn "Use generic-lens or generic-optics with 'targetArn' instead"  #-}

instance Core.FromJSON Destination where
        parseJSON
          = Core.withObject "Destination" Core.$
              \ x ->
                Destination' Core.<$>
                  (x Core..:? "accessPolicy") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "creationTime"
                    Core.<*> x Core..:? "destinationName"
                    Core.<*> x Core..:? "roleArn"
                    Core.<*> x Core..:? "targetArn"
