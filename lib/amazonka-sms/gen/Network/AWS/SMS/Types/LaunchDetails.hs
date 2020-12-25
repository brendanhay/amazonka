{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.LaunchDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.LaunchDetails
  ( LaunchDetails (..),

    -- * Smart constructor
    mkLaunchDetails,

    -- * Lenses
    ldLatestLaunchTime,
    ldStackId,
    ldStackName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.StackId as Types
import qualified Network.AWS.SMS.Types.StackName as Types

-- | Details about the latest launch of an application.
--
-- /See:/ 'mkLaunchDetails' smart constructor.
data LaunchDetails = LaunchDetails'
  { -- | The latest time that this application was launched successfully.
    latestLaunchTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the latest stack launched for this application.
    stackId :: Core.Maybe Types.StackId,
    -- | The name of the latest stack launched for this application.
    stackName :: Core.Maybe Types.StackName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LaunchDetails' value with any optional fields omitted.
mkLaunchDetails ::
  LaunchDetails
mkLaunchDetails =
  LaunchDetails'
    { latestLaunchTime = Core.Nothing,
      stackId = Core.Nothing,
      stackName = Core.Nothing
    }

-- | The latest time that this application was launched successfully.
--
-- /Note:/ Consider using 'latestLaunchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldLatestLaunchTime :: Lens.Lens' LaunchDetails (Core.Maybe Core.NominalDiffTime)
ldLatestLaunchTime = Lens.field @"latestLaunchTime"
{-# DEPRECATED ldLatestLaunchTime "Use generic-lens or generic-optics with 'latestLaunchTime' instead." #-}

-- | The ID of the latest stack launched for this application.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStackId :: Lens.Lens' LaunchDetails (Core.Maybe Types.StackId)
ldStackId = Lens.field @"stackId"
{-# DEPRECATED ldStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The name of the latest stack launched for this application.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldStackName :: Lens.Lens' LaunchDetails (Core.Maybe Types.StackName)
ldStackName = Lens.field @"stackName"
{-# DEPRECATED ldStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Core.FromJSON LaunchDetails where
  parseJSON =
    Core.withObject "LaunchDetails" Core.$
      \x ->
        LaunchDetails'
          Core.<$> (x Core..:? "latestLaunchTime")
          Core.<*> (x Core..:? "stackId")
          Core.<*> (x Core..:? "stackName")
