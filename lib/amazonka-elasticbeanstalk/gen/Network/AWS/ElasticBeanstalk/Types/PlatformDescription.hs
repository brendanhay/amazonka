{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformDescription
  ( PlatformDescription (..),

    -- * Smart constructor
    mkPlatformDescription,

    -- * Lenses
    pdPlatformBranchName,
    pdSupportedAddonList,
    pdPlatformCategory,
    pdPlatformBranchLifecycleState,
    pdPlatformVersion,
    pdPlatformStatus,
    pdMaintainer,
    pdPlatformLifecycleState,
    pdPlatformOwner,
    pdDateUpdated,
    pdCustomAMIList,
    pdDateCreated,
    pdOperatingSystemName,
    pdFrameworks,
    pdPlatformARN,
    pdOperatingSystemVersion,
    pdProgrammingLanguages,
    pdSolutionStackName,
    pdPlatformName,
    pdDescription,
    pdSupportedTierList,
  )
where

import Network.AWS.ElasticBeanstalk.Types.CustomAMI
import Network.AWS.ElasticBeanstalk.Types.PlatformFramework
import Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Network.AWS.ElasticBeanstalk.Types.PlatformStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about a platform version.
--
-- /See:/ 'mkPlatformDescription' smart constructor.
data PlatformDescription = PlatformDescription'
  { platformBranchName ::
      Lude.Maybe Lude.Text,
    supportedAddonList :: Lude.Maybe [Lude.Text],
    platformCategory :: Lude.Maybe Lude.Text,
    platformBranchLifecycleState ::
      Lude.Maybe Lude.Text,
    platformVersion :: Lude.Maybe Lude.Text,
    platformStatus :: Lude.Maybe PlatformStatus,
    maintainer :: Lude.Maybe Lude.Text,
    platformLifecycleState :: Lude.Maybe Lude.Text,
    platformOwner :: Lude.Maybe Lude.Text,
    dateUpdated :: Lude.Maybe Lude.DateTime,
    customAMIList :: Lude.Maybe [CustomAMI],
    dateCreated :: Lude.Maybe Lude.DateTime,
    operatingSystemName :: Lude.Maybe Lude.Text,
    frameworks :: Lude.Maybe [PlatformFramework],
    platformARN :: Lude.Maybe Lude.Text,
    operatingSystemVersion :: Lude.Maybe Lude.Text,
    programmingLanguages ::
      Lude.Maybe [PlatformProgrammingLanguage],
    solutionStackName :: Lude.Maybe Lude.Text,
    platformName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    supportedTierList :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformDescription' with the minimum fields required to make a request.
--
-- * 'customAMIList' - The custom AMIs supported by the platform version.
-- * 'dateCreated' - The date when the platform version was created.
-- * 'dateUpdated' - The date when the platform version was last updated.
-- * 'description' - The description of the platform version.
-- * 'frameworks' - The frameworks supported by the platform version.
-- * 'maintainer' - Information about the maintainer of the platform version.
-- * 'operatingSystemName' - The operating system used by the platform version.
-- * 'operatingSystemVersion' - The version of the operating system used by the platform version.
-- * 'platformARN' - The ARN of the platform version.
-- * 'platformBranchLifecycleState' - The state of the platform version's branch in its lifecycle.
--
-- Possible values: @Beta@ | @Supported@ | @Deprecated@ | @Retired@
-- * 'platformBranchName' - The platform branch to which the platform version belongs.
-- * 'platformCategory' - The category of the platform version.
-- * 'platformLifecycleState' - The state of the platform version in its lifecycle.
--
-- Possible values: @Recommended@ | @null@
-- If a null value is returned, the platform version isn't the recommended one for its branch. Each platform branch has a single recommended platform version, typically the most recent one.
-- * 'platformName' - The name of the platform version.
-- * 'platformOwner' - The AWS account ID of the person who created the platform version.
-- * 'platformStatus' - The status of the platform version.
-- * 'platformVersion' - The version of the platform version.
-- * 'programmingLanguages' - The programming languages supported by the platform version.
-- * 'solutionStackName' - The name of the solution stack used by the platform version.
-- * 'supportedAddonList' - The additions supported by the platform version.
-- * 'supportedTierList' - The tiers supported by the platform version.
mkPlatformDescription ::
  PlatformDescription
mkPlatformDescription =
  PlatformDescription'
    { platformBranchName = Lude.Nothing,
      supportedAddonList = Lude.Nothing,
      platformCategory = Lude.Nothing,
      platformBranchLifecycleState = Lude.Nothing,
      platformVersion = Lude.Nothing,
      platformStatus = Lude.Nothing,
      maintainer = Lude.Nothing,
      platformLifecycleState = Lude.Nothing,
      platformOwner = Lude.Nothing,
      dateUpdated = Lude.Nothing,
      customAMIList = Lude.Nothing,
      dateCreated = Lude.Nothing,
      operatingSystemName = Lude.Nothing,
      frameworks = Lude.Nothing,
      platformARN = Lude.Nothing,
      operatingSystemVersion = Lude.Nothing,
      programmingLanguages = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      platformName = Lude.Nothing,
      description = Lude.Nothing,
      supportedTierList = Lude.Nothing
    }

-- | The platform branch to which the platform version belongs.
--
-- /Note:/ Consider using 'platformBranchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformBranchName :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformBranchName = Lens.lens (platformBranchName :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformBranchName = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformBranchName "Use generic-lens or generic-optics with 'platformBranchName' instead." #-}

-- | The additions supported by the platform version.
--
-- /Note:/ Consider using 'supportedAddonList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdSupportedAddonList :: Lens.Lens' PlatformDescription (Lude.Maybe [Lude.Text])
pdSupportedAddonList = Lens.lens (supportedAddonList :: PlatformDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedAddonList = a} :: PlatformDescription)
{-# DEPRECATED pdSupportedAddonList "Use generic-lens or generic-optics with 'supportedAddonList' instead." #-}

-- | The category of the platform version.
--
-- /Note:/ Consider using 'platformCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformCategory :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformCategory = Lens.lens (platformCategory :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformCategory = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformCategory "Use generic-lens or generic-optics with 'platformCategory' instead." #-}

-- | The state of the platform version's branch in its lifecycle.
--
-- Possible values: @Beta@ | @Supported@ | @Deprecated@ | @Retired@
--
-- /Note:/ Consider using 'platformBranchLifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformBranchLifecycleState :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformBranchLifecycleState = Lens.lens (platformBranchLifecycleState :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformBranchLifecycleState = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformBranchLifecycleState "Use generic-lens or generic-optics with 'platformBranchLifecycleState' instead." #-}

-- | The version of the platform version.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformVersion :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformVersion = Lens.lens (platformVersion :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The status of the platform version.
--
-- /Note:/ Consider using 'platformStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformStatus :: Lens.Lens' PlatformDescription (Lude.Maybe PlatformStatus)
pdPlatformStatus = Lens.lens (platformStatus :: PlatformDescription -> Lude.Maybe PlatformStatus) (\s a -> s {platformStatus = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformStatus "Use generic-lens or generic-optics with 'platformStatus' instead." #-}

-- | Information about the maintainer of the platform version.
--
-- /Note:/ Consider using 'maintainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdMaintainer :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdMaintainer = Lens.lens (maintainer :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {maintainer = a} :: PlatformDescription)
{-# DEPRECATED pdMaintainer "Use generic-lens or generic-optics with 'maintainer' instead." #-}

-- | The state of the platform version in its lifecycle.
--
-- Possible values: @Recommended@ | @null@
-- If a null value is returned, the platform version isn't the recommended one for its branch. Each platform branch has a single recommended platform version, typically the most recent one.
--
-- /Note:/ Consider using 'platformLifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformLifecycleState :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformLifecycleState = Lens.lens (platformLifecycleState :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformLifecycleState = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformLifecycleState "Use generic-lens or generic-optics with 'platformLifecycleState' instead." #-}

-- | The AWS account ID of the person who created the platform version.
--
-- /Note:/ Consider using 'platformOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformOwner :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformOwner = Lens.lens (platformOwner :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformOwner = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformOwner "Use generic-lens or generic-optics with 'platformOwner' instead." #-}

-- | The date when the platform version was last updated.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDateUpdated :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.DateTime)
pdDateUpdated = Lens.lens (dateUpdated :: PlatformDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateUpdated = a} :: PlatformDescription)
{-# DEPRECATED pdDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The custom AMIs supported by the platform version.
--
-- /Note:/ Consider using 'customAMIList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCustomAMIList :: Lens.Lens' PlatformDescription (Lude.Maybe [CustomAMI])
pdCustomAMIList = Lens.lens (customAMIList :: PlatformDescription -> Lude.Maybe [CustomAMI]) (\s a -> s {customAMIList = a} :: PlatformDescription)
{-# DEPRECATED pdCustomAMIList "Use generic-lens or generic-optics with 'customAMIList' instead." #-}

-- | The date when the platform version was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDateCreated :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.DateTime)
pdDateCreated = Lens.lens (dateCreated :: PlatformDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateCreated = a} :: PlatformDescription)
{-# DEPRECATED pdDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The operating system used by the platform version.
--
-- /Note:/ Consider using 'operatingSystemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdOperatingSystemName :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdOperatingSystemName = Lens.lens (operatingSystemName :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {operatingSystemName = a} :: PlatformDescription)
{-# DEPRECATED pdOperatingSystemName "Use generic-lens or generic-optics with 'operatingSystemName' instead." #-}

-- | The frameworks supported by the platform version.
--
-- /Note:/ Consider using 'frameworks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdFrameworks :: Lens.Lens' PlatformDescription (Lude.Maybe [PlatformFramework])
pdFrameworks = Lens.lens (frameworks :: PlatformDescription -> Lude.Maybe [PlatformFramework]) (\s a -> s {frameworks = a} :: PlatformDescription)
{-# DEPRECATED pdFrameworks "Use generic-lens or generic-optics with 'frameworks' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformARN :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformARN = Lens.lens (platformARN :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | The version of the operating system used by the platform version.
--
-- /Note:/ Consider using 'operatingSystemVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdOperatingSystemVersion :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdOperatingSystemVersion = Lens.lens (operatingSystemVersion :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {operatingSystemVersion = a} :: PlatformDescription)
{-# DEPRECATED pdOperatingSystemVersion "Use generic-lens or generic-optics with 'operatingSystemVersion' instead." #-}

-- | The programming languages supported by the platform version.
--
-- /Note:/ Consider using 'programmingLanguages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProgrammingLanguages :: Lens.Lens' PlatformDescription (Lude.Maybe [PlatformProgrammingLanguage])
pdProgrammingLanguages = Lens.lens (programmingLanguages :: PlatformDescription -> Lude.Maybe [PlatformProgrammingLanguage]) (\s a -> s {programmingLanguages = a} :: PlatformDescription)
{-# DEPRECATED pdProgrammingLanguages "Use generic-lens or generic-optics with 'programmingLanguages' instead." #-}

-- | The name of the solution stack used by the platform version.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdSolutionStackName :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdSolutionStackName = Lens.lens (solutionStackName :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: PlatformDescription)
{-# DEPRECATED pdSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The name of the platform version.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPlatformName :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdPlatformName = Lens.lens (platformName :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformName = a} :: PlatformDescription)
{-# DEPRECATED pdPlatformName "Use generic-lens or generic-optics with 'platformName' instead." #-}

-- | The description of the platform version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PlatformDescription (Lude.Maybe Lude.Text)
pdDescription = Lens.lens (description :: PlatformDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PlatformDescription)
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tiers supported by the platform version.
--
-- /Note:/ Consider using 'supportedTierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdSupportedTierList :: Lens.Lens' PlatformDescription (Lude.Maybe [Lude.Text])
pdSupportedTierList = Lens.lens (supportedTierList :: PlatformDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedTierList = a} :: PlatformDescription)
{-# DEPRECATED pdSupportedTierList "Use generic-lens or generic-optics with 'supportedTierList' instead." #-}

instance Lude.FromXML PlatformDescription where
  parseXML x =
    PlatformDescription'
      Lude.<$> (x Lude..@? "PlatformBranchName")
      Lude.<*> ( x Lude..@? "SupportedAddonList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "PlatformCategory")
      Lude.<*> (x Lude..@? "PlatformBranchLifecycleState")
      Lude.<*> (x Lude..@? "PlatformVersion")
      Lude.<*> (x Lude..@? "PlatformStatus")
      Lude.<*> (x Lude..@? "Maintainer")
      Lude.<*> (x Lude..@? "PlatformLifecycleState")
      Lude.<*> (x Lude..@? "PlatformOwner")
      Lude.<*> (x Lude..@? "DateUpdated")
      Lude.<*> ( x Lude..@? "CustomAmiList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DateCreated")
      Lude.<*> (x Lude..@? "OperatingSystemName")
      Lude.<*> ( x Lude..@? "Frameworks" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "PlatformArn")
      Lude.<*> (x Lude..@? "OperatingSystemVersion")
      Lude.<*> ( x Lude..@? "ProgrammingLanguages" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "SolutionStackName")
      Lude.<*> (x Lude..@? "PlatformName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "SupportedTierList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
