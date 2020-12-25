{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceTaskDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceTaskDetails
  ( ImportInstanceTaskDetails (..),

    -- * Smart constructor
    mkImportInstanceTaskDetails,

    -- * Lenses
    iitdDescription,
    iitdInstanceId,
    iitdPlatform,
    iitdVolumes,
  )
where

import qualified Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem as Types
import qualified Network.AWS.EC2.Types.PlatformValues as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import instance task.
--
-- /See:/ 'mkImportInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { -- | A description of the task.
    description :: Core.Maybe Types.String,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String,
    -- | The instance operating system.
    platform :: Core.Maybe Types.PlatformValues,
    -- | The volumes.
    volumes :: Core.Maybe [Types.ImportInstanceVolumeDetailItem]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstanceTaskDetails' value with any optional fields omitted.
mkImportInstanceTaskDetails ::
  ImportInstanceTaskDetails
mkImportInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { description = Core.Nothing,
      instanceId = Core.Nothing,
      platform = Core.Nothing,
      volumes = Core.Nothing
    }

-- | A description of the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdDescription :: Lens.Lens' ImportInstanceTaskDetails (Core.Maybe Types.String)
iitdDescription = Lens.field @"description"
{-# DEPRECATED iitdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdInstanceId :: Lens.Lens' ImportInstanceTaskDetails (Core.Maybe Types.String)
iitdInstanceId = Lens.field @"instanceId"
{-# DEPRECATED iitdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance operating system.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdPlatform :: Lens.Lens' ImportInstanceTaskDetails (Core.Maybe Types.PlatformValues)
iitdPlatform = Lens.field @"platform"
{-# DEPRECATED iitdPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The volumes.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdVolumes :: Lens.Lens' ImportInstanceTaskDetails (Core.Maybe [Types.ImportInstanceVolumeDetailItem])
iitdVolumes = Lens.field @"volumes"
{-# DEPRECATED iitdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

instance Core.FromXML ImportInstanceTaskDetails where
  parseXML x =
    ImportInstanceTaskDetails'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "platform")
      Core.<*> (x Core..@? "volumes" Core..<@> Core.parseXMLList "item")
