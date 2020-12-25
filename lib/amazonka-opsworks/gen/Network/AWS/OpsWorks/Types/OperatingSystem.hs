{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.OperatingSystem
  ( OperatingSystem (..),

    -- * Smart constructor
    mkOperatingSystem,

    -- * Lenses
    osConfigurationManagers,
    osId,
    osName,
    osReportedName,
    osReportedVersion,
    osSupported,
    osType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'mkOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { -- | Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
    configurationManagers :: Core.Maybe [Types.OperatingSystemConfigurationManager],
    -- | The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
    id :: Core.Maybe Types.String,
    -- | The name of the operating system, such as @Amazon Linux 2018.03@ .
    name :: Core.Maybe Types.String,
    -- | A short name for the operating system manufacturer.
    reportedName :: Core.Maybe Types.String,
    -- | The version of the operating system, including the release and edition, if applicable.
    reportedVersion :: Core.Maybe Types.String,
    -- | Indicates that an operating system is not supported for new instances.
    supported :: Core.Maybe Core.Bool,
    -- | The type of a supported operating system, either @Linux@ or @Windows@ .
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OperatingSystem' value with any optional fields omitted.
mkOperatingSystem ::
  OperatingSystem
mkOperatingSystem =
  OperatingSystem'
    { configurationManagers = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      reportedName = Core.Nothing,
      reportedVersion = Core.Nothing,
      supported = Core.Nothing,
      type' = Core.Nothing
    }

-- | Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
--
-- /Note:/ Consider using 'configurationManagers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osConfigurationManagers :: Lens.Lens' OperatingSystem (Core.Maybe [Types.OperatingSystemConfigurationManager])
osConfigurationManagers = Lens.field @"configurationManagers"
{-# DEPRECATED osConfigurationManagers "Use generic-lens or generic-optics with 'configurationManagers' instead." #-}

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osId :: Lens.Lens' OperatingSystem (Core.Maybe Types.String)
osId = Lens.field @"id"
{-# DEPRECATED osId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osName :: Lens.Lens' OperatingSystem (Core.Maybe Types.String)
osName = Lens.field @"name"
{-# DEPRECATED osName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A short name for the operating system manufacturer.
--
-- /Note:/ Consider using 'reportedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedName :: Lens.Lens' OperatingSystem (Core.Maybe Types.String)
osReportedName = Lens.field @"reportedName"
{-# DEPRECATED osReportedName "Use generic-lens or generic-optics with 'reportedName' instead." #-}

-- | The version of the operating system, including the release and edition, if applicable.
--
-- /Note:/ Consider using 'reportedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedVersion :: Lens.Lens' OperatingSystem (Core.Maybe Types.String)
osReportedVersion = Lens.field @"reportedVersion"
{-# DEPRECATED osReportedVersion "Use generic-lens or generic-optics with 'reportedVersion' instead." #-}

-- | Indicates that an operating system is not supported for new instances.
--
-- /Note:/ Consider using 'supported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSupported :: Lens.Lens' OperatingSystem (Core.Maybe Core.Bool)
osSupported = Lens.field @"supported"
{-# DEPRECATED osSupported "Use generic-lens or generic-optics with 'supported' instead." #-}

-- | The type of a supported operating system, either @Linux@ or @Windows@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperatingSystem (Core.Maybe Types.String)
osType = Lens.field @"type'"
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON OperatingSystem where
  parseJSON =
    Core.withObject "OperatingSystem" Core.$
      \x ->
        OperatingSystem'
          Core.<$> (x Core..:? "ConfigurationManagers")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "ReportedName")
          Core.<*> (x Core..:? "ReportedVersion")
          Core.<*> (x Core..:? "Supported")
          Core.<*> (x Core..:? "Type")
