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
    osReportedVersion,
    osSupported,
    osName,
    osId,
    osConfigurationManagers,
    osType,
    osReportedName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import qualified Network.AWS.Prelude as Lude

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
-- /See:/ 'mkOperatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { reportedVersion ::
      Lude.Maybe Lude.Text,
    supported :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    configurationManagers ::
      Lude.Maybe [OperatingSystemConfigurationManager],
    type' :: Lude.Maybe Lude.Text,
    reportedName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OperatingSystem' with the minimum fields required to make a request.
--
-- * 'configurationManagers' - Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
-- * 'id' - The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
-- * 'name' - The name of the operating system, such as @Amazon Linux 2018.03@ .
-- * 'reportedName' - A short name for the operating system manufacturer.
-- * 'reportedVersion' - The version of the operating system, including the release and edition, if applicable.
-- * 'supported' - Indicates that an operating system is not supported for new instances.
-- * 'type'' - The type of a supported operating system, either @Linux@ or @Windows@ .
mkOperatingSystem ::
  OperatingSystem
mkOperatingSystem =
  OperatingSystem'
    { reportedVersion = Lude.Nothing,
      supported = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      configurationManagers = Lude.Nothing,
      type' = Lude.Nothing,
      reportedName = Lude.Nothing
    }

-- | The version of the operating system, including the release and edition, if applicable.
--
-- /Note:/ Consider using 'reportedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedVersion :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Text)
osReportedVersion = Lens.lens (reportedVersion :: OperatingSystem -> Lude.Maybe Lude.Text) (\s a -> s {reportedVersion = a} :: OperatingSystem)
{-# DEPRECATED osReportedVersion "Use generic-lens or generic-optics with 'reportedVersion' instead." #-}

-- | Indicates that an operating system is not supported for new instances.
--
-- /Note:/ Consider using 'supported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSupported :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Bool)
osSupported = Lens.lens (supported :: OperatingSystem -> Lude.Maybe Lude.Bool) (\s a -> s {supported = a} :: OperatingSystem)
{-# DEPRECATED osSupported "Use generic-lens or generic-optics with 'supported' instead." #-}

-- | The name of the operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osName :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Text)
osName = Lens.lens (name :: OperatingSystem -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OperatingSystem)
{-# DEPRECATED osName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of a supported operating system, such as @Amazon Linux 2018.03@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osId :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Text)
osId = Lens.lens (id :: OperatingSystem -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OperatingSystem)
{-# DEPRECATED osId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
--
-- /Note:/ Consider using 'configurationManagers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osConfigurationManagers :: Lens.Lens' OperatingSystem (Lude.Maybe [OperatingSystemConfigurationManager])
osConfigurationManagers = Lens.lens (configurationManagers :: OperatingSystem -> Lude.Maybe [OperatingSystemConfigurationManager]) (\s a -> s {configurationManagers = a} :: OperatingSystem)
{-# DEPRECATED osConfigurationManagers "Use generic-lens or generic-optics with 'configurationManagers' instead." #-}

-- | The type of a supported operating system, either @Linux@ or @Windows@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Text)
osType = Lens.lens (type' :: OperatingSystem -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: OperatingSystem)
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A short name for the operating system manufacturer.
--
-- /Note:/ Consider using 'reportedName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReportedName :: Lens.Lens' OperatingSystem (Lude.Maybe Lude.Text)
osReportedName = Lens.lens (reportedName :: OperatingSystem -> Lude.Maybe Lude.Text) (\s a -> s {reportedName = a} :: OperatingSystem)
{-# DEPRECATED osReportedName "Use generic-lens or generic-optics with 'reportedName' instead." #-}

instance Lude.FromJSON OperatingSystem where
  parseJSON =
    Lude.withObject
      "OperatingSystem"
      ( \x ->
          OperatingSystem'
            Lude.<$> (x Lude..:? "ReportedVersion")
            Lude.<*> (x Lude..:? "Supported")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "ConfigurationManagers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "ReportedName")
      )
