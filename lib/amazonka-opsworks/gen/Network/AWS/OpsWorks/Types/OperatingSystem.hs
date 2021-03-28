{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.OperatingSystem
  ( OperatingSystem (..)
  -- * Smart constructor
  , mkOperatingSystem
  -- * Lenses
  , osConfigurationManagers
  , osId
  , osName
  , osReportedName
  , osReportedVersion
  , osSupported
  , osType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager as Types
import qualified Network.AWS.Prelude as Core

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'mkOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { configurationManagers :: Core.Maybe [Types.OperatingSystemConfigurationManager]
    -- ^ Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
  , name :: Core.Maybe Core.Text
    -- ^ The name of the operating system, such as @Amazon Linux 2018.03@ .
  , reportedName :: Core.Maybe Core.Text
    -- ^ A short name for the operating system manufacturer.
  , reportedVersion :: Core.Maybe Core.Text
    -- ^ The version of the operating system, including the release and edition, if applicable.
  , supported :: Core.Maybe Core.Bool
    -- ^ Indicates that an operating system is not supported for new instances.
  , type' :: Core.Maybe Core.Text
    -- ^ The type of a supported operating system, either @Linux@ or @Windows@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OperatingSystem' value with any optional fields omitted.
mkOperatingSystem
    :: OperatingSystem
mkOperatingSystem
  = OperatingSystem'{configurationManagers = Core.Nothing,
                     id = Core.Nothing, name = Core.Nothing,
                     reportedName = Core.Nothing, reportedVersion = Core.Nothing,
                     supported = Core.Nothing, type' = Core.Nothing}

-- | Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
--
-- /Note:/ Consider using 'configurationManagers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osConfigurationManagers :: Lens.Lens' OperatingSystem (Core.Maybe [Types.OperatingSystemConfigurationManager])
osConfigurationManagers = Lens.field @"configurationManagers"
{-# INLINEABLE osConfigurationManagers #-}
{-# DEPRECATED configurationManagers "Use generic-lens or generic-optics with 'configurationManagers' instead"  #-}

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osId :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
osId = Lens.field @"id"
{-# INLINEABLE osId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osName :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
osName = Lens.field @"name"
{-# INLINEABLE osName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A short name for the operating system manufacturer.
--
-- /Note:/ Consider using 'reportedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedName :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
osReportedName = Lens.field @"reportedName"
{-# INLINEABLE osReportedName #-}
{-# DEPRECATED reportedName "Use generic-lens or generic-optics with 'reportedName' instead"  #-}

-- | The version of the operating system, including the release and edition, if applicable.
--
-- /Note:/ Consider using 'reportedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedVersion :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
osReportedVersion = Lens.field @"reportedVersion"
{-# INLINEABLE osReportedVersion #-}
{-# DEPRECATED reportedVersion "Use generic-lens or generic-optics with 'reportedVersion' instead"  #-}

-- | Indicates that an operating system is not supported for new instances.
--
-- /Note:/ Consider using 'supported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSupported :: Lens.Lens' OperatingSystem (Core.Maybe Core.Bool)
osSupported = Lens.field @"supported"
{-# INLINEABLE osSupported #-}
{-# DEPRECATED supported "Use generic-lens or generic-optics with 'supported' instead"  #-}

-- | The type of a supported operating system, either @Linux@ or @Windows@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperatingSystem (Core.Maybe Core.Text)
osType = Lens.field @"type'"
{-# INLINEABLE osType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON OperatingSystem where
        parseJSON
          = Core.withObject "OperatingSystem" Core.$
              \ x ->
                OperatingSystem' Core.<$>
                  (x Core..:? "ConfigurationManagers") Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "ReportedName"
                    Core.<*> x Core..:? "ReportedVersion"
                    Core.<*> x Core..:? "Supported"
                    Core.<*> x Core..:? "Type"
