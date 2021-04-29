{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformSummary where

import Network.AWS.ElasticBeanstalk.Types.PlatformStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about a platform version.
--
-- /See:/ 'newPlatformSummary' smart constructor.
data PlatformSummary = PlatformSummary'
  { -- | The category of platform version.
    platformCategory :: Prelude.Maybe Prelude.Text,
    -- | The operating system used by the platform version.
    operatingSystemName :: Prelude.Maybe Prelude.Text,
    -- | The platform branch to which the platform version belongs.
    platformBranchName :: Prelude.Maybe Prelude.Text,
    -- | The additions associated with the platform version.
    supportedAddonList :: Prelude.Maybe [Prelude.Text],
    -- | The AWS account ID of the person who created the platform version.
    platformOwner :: Prelude.Maybe Prelude.Text,
    -- | The status of the platform version. You can create an environment from
    -- the platform version once it is ready.
    platformStatus :: Prelude.Maybe PlatformStatus,
    -- | The version string of the platform version.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The state of the platform version\'s branch in its lifecycle.
    --
    -- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
    platformBranchLifecycleState :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The tiers in which the platform version runs.
    supportedTierList :: Prelude.Maybe [Prelude.Text],
    -- | The state of the platform version in its lifecycle.
    --
    -- Possible values: @recommended@ | empty
    --
    -- If an empty value is returned, the platform version is supported but
    -- isn\'t the recommended one for its branch.
    platformLifecycleState :: Prelude.Maybe Prelude.Text,
    -- | The version of the operating system used by the platform version.
    operatingSystemVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PlatformSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformCategory', 'platformSummary_platformCategory' - The category of platform version.
--
-- 'operatingSystemName', 'platformSummary_operatingSystemName' - The operating system used by the platform version.
--
-- 'platformBranchName', 'platformSummary_platformBranchName' - The platform branch to which the platform version belongs.
--
-- 'supportedAddonList', 'platformSummary_supportedAddonList' - The additions associated with the platform version.
--
-- 'platformOwner', 'platformSummary_platformOwner' - The AWS account ID of the person who created the platform version.
--
-- 'platformStatus', 'platformSummary_platformStatus' - The status of the platform version. You can create an environment from
-- the platform version once it is ready.
--
-- 'platformVersion', 'platformSummary_platformVersion' - The version string of the platform version.
--
-- 'platformBranchLifecycleState', 'platformSummary_platformBranchLifecycleState' - The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- 'platformArn', 'platformSummary_platformArn' - The ARN of the platform version.
--
-- 'supportedTierList', 'platformSummary_supportedTierList' - The tiers in which the platform version runs.
--
-- 'platformLifecycleState', 'platformSummary_platformLifecycleState' - The state of the platform version in its lifecycle.
--
-- Possible values: @recommended@ | empty
--
-- If an empty value is returned, the platform version is supported but
-- isn\'t the recommended one for its branch.
--
-- 'operatingSystemVersion', 'platformSummary_operatingSystemVersion' - The version of the operating system used by the platform version.
newPlatformSummary ::
  PlatformSummary
newPlatformSummary =
  PlatformSummary'
    { platformCategory =
        Prelude.Nothing,
      operatingSystemName = Prelude.Nothing,
      platformBranchName = Prelude.Nothing,
      supportedAddonList = Prelude.Nothing,
      platformOwner = Prelude.Nothing,
      platformStatus = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      platformBranchLifecycleState = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      supportedTierList = Prelude.Nothing,
      platformLifecycleState = Prelude.Nothing,
      operatingSystemVersion = Prelude.Nothing
    }

-- | The category of platform version.
platformSummary_platformCategory :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformCategory = Lens.lens (\PlatformSummary' {platformCategory} -> platformCategory) (\s@PlatformSummary' {} a -> s {platformCategory = a} :: PlatformSummary)

-- | The operating system used by the platform version.
platformSummary_operatingSystemName :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_operatingSystemName = Lens.lens (\PlatformSummary' {operatingSystemName} -> operatingSystemName) (\s@PlatformSummary' {} a -> s {operatingSystemName = a} :: PlatformSummary)

-- | The platform branch to which the platform version belongs.
platformSummary_platformBranchName :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformBranchName = Lens.lens (\PlatformSummary' {platformBranchName} -> platformBranchName) (\s@PlatformSummary' {} a -> s {platformBranchName = a} :: PlatformSummary)

-- | The additions associated with the platform version.
platformSummary_supportedAddonList :: Lens.Lens' PlatformSummary (Prelude.Maybe [Prelude.Text])
platformSummary_supportedAddonList = Lens.lens (\PlatformSummary' {supportedAddonList} -> supportedAddonList) (\s@PlatformSummary' {} a -> s {supportedAddonList = a} :: PlatformSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS account ID of the person who created the platform version.
platformSummary_platformOwner :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformOwner = Lens.lens (\PlatformSummary' {platformOwner} -> platformOwner) (\s@PlatformSummary' {} a -> s {platformOwner = a} :: PlatformSummary)

-- | The status of the platform version. You can create an environment from
-- the platform version once it is ready.
platformSummary_platformStatus :: Lens.Lens' PlatformSummary (Prelude.Maybe PlatformStatus)
platformSummary_platformStatus = Lens.lens (\PlatformSummary' {platformStatus} -> platformStatus) (\s@PlatformSummary' {} a -> s {platformStatus = a} :: PlatformSummary)

-- | The version string of the platform version.
platformSummary_platformVersion :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformVersion = Lens.lens (\PlatformSummary' {platformVersion} -> platformVersion) (\s@PlatformSummary' {} a -> s {platformVersion = a} :: PlatformSummary)

-- | The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
platformSummary_platformBranchLifecycleState :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformBranchLifecycleState = Lens.lens (\PlatformSummary' {platformBranchLifecycleState} -> platformBranchLifecycleState) (\s@PlatformSummary' {} a -> s {platformBranchLifecycleState = a} :: PlatformSummary)

-- | The ARN of the platform version.
platformSummary_platformArn :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformArn = Lens.lens (\PlatformSummary' {platformArn} -> platformArn) (\s@PlatformSummary' {} a -> s {platformArn = a} :: PlatformSummary)

-- | The tiers in which the platform version runs.
platformSummary_supportedTierList :: Lens.Lens' PlatformSummary (Prelude.Maybe [Prelude.Text])
platformSummary_supportedTierList = Lens.lens (\PlatformSummary' {supportedTierList} -> supportedTierList) (\s@PlatformSummary' {} a -> s {supportedTierList = a} :: PlatformSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The state of the platform version in its lifecycle.
--
-- Possible values: @recommended@ | empty
--
-- If an empty value is returned, the platform version is supported but
-- isn\'t the recommended one for its branch.
platformSummary_platformLifecycleState :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformLifecycleState = Lens.lens (\PlatformSummary' {platformLifecycleState} -> platformLifecycleState) (\s@PlatformSummary' {} a -> s {platformLifecycleState = a} :: PlatformSummary)

-- | The version of the operating system used by the platform version.
platformSummary_operatingSystemVersion :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_operatingSystemVersion = Lens.lens (\PlatformSummary' {operatingSystemVersion} -> operatingSystemVersion) (\s@PlatformSummary' {} a -> s {operatingSystemVersion = a} :: PlatformSummary)

instance Prelude.FromXML PlatformSummary where
  parseXML x =
    PlatformSummary'
      Prelude.<$> (x Prelude..@? "PlatformCategory")
      Prelude.<*> (x Prelude..@? "OperatingSystemName")
      Prelude.<*> (x Prelude..@? "PlatformBranchName")
      Prelude.<*> ( x Prelude..@? "SupportedAddonList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "PlatformOwner")
      Prelude.<*> (x Prelude..@? "PlatformStatus")
      Prelude.<*> (x Prelude..@? "PlatformVersion")
      Prelude.<*> (x Prelude..@? "PlatformBranchLifecycleState")
      Prelude.<*> (x Prelude..@? "PlatformArn")
      Prelude.<*> ( x Prelude..@? "SupportedTierList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "PlatformLifecycleState")
      Prelude.<*> (x Prelude..@? "OperatingSystemVersion")

instance Prelude.Hashable PlatformSummary

instance Prelude.NFData PlatformSummary
