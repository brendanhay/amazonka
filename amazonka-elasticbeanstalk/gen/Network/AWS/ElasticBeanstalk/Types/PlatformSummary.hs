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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.PlatformStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about a platform version.
--
-- /See:/ 'newPlatformSummary' smart constructor.
data PlatformSummary = PlatformSummary'
  { -- | The operating system used by the platform version.
    operatingSystemName :: Prelude.Maybe Prelude.Text,
    -- | The category of platform version.
    platformCategory :: Prelude.Maybe Prelude.Text,
    -- | The additions associated with the platform version.
    supportedAddonList :: Prelude.Maybe [Prelude.Text],
    -- | The platform branch to which the platform version belongs.
    platformBranchName :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the person who created the platform version.
    platformOwner :: Prelude.Maybe Prelude.Text,
    -- | The status of the platform version. You can create an environment from
    -- the platform version once it is ready.
    platformStatus :: Prelude.Maybe PlatformStatus,
    -- | The version string of the platform version.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the platform version\'s branch in its lifecycle.
    --
    -- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
    platformBranchLifecycleState :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystemName', 'platformSummary_operatingSystemName' - The operating system used by the platform version.
--
-- 'platformCategory', 'platformSummary_platformCategory' - The category of platform version.
--
-- 'supportedAddonList', 'platformSummary_supportedAddonList' - The additions associated with the platform version.
--
-- 'platformBranchName', 'platformSummary_platformBranchName' - The platform branch to which the platform version belongs.
--
-- 'platformOwner', 'platformSummary_platformOwner' - The AWS account ID of the person who created the platform version.
--
-- 'platformStatus', 'platformSummary_platformStatus' - The status of the platform version. You can create an environment from
-- the platform version once it is ready.
--
-- 'platformVersion', 'platformSummary_platformVersion' - The version string of the platform version.
--
-- 'platformArn', 'platformSummary_platformArn' - The ARN of the platform version.
--
-- 'platformBranchLifecycleState', 'platformSummary_platformBranchLifecycleState' - The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
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
    { operatingSystemName =
        Prelude.Nothing,
      platformCategory = Prelude.Nothing,
      supportedAddonList = Prelude.Nothing,
      platformBranchName = Prelude.Nothing,
      platformOwner = Prelude.Nothing,
      platformStatus = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      platformBranchLifecycleState = Prelude.Nothing,
      supportedTierList = Prelude.Nothing,
      platformLifecycleState = Prelude.Nothing,
      operatingSystemVersion = Prelude.Nothing
    }

-- | The operating system used by the platform version.
platformSummary_operatingSystemName :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_operatingSystemName = Lens.lens (\PlatformSummary' {operatingSystemName} -> operatingSystemName) (\s@PlatformSummary' {} a -> s {operatingSystemName = a} :: PlatformSummary)

-- | The category of platform version.
platformSummary_platformCategory :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformCategory = Lens.lens (\PlatformSummary' {platformCategory} -> platformCategory) (\s@PlatformSummary' {} a -> s {platformCategory = a} :: PlatformSummary)

-- | The additions associated with the platform version.
platformSummary_supportedAddonList :: Lens.Lens' PlatformSummary (Prelude.Maybe [Prelude.Text])
platformSummary_supportedAddonList = Lens.lens (\PlatformSummary' {supportedAddonList} -> supportedAddonList) (\s@PlatformSummary' {} a -> s {supportedAddonList = a} :: PlatformSummary) Prelude.. Lens.mapping Lens._Coerce

-- | The platform branch to which the platform version belongs.
platformSummary_platformBranchName :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformBranchName = Lens.lens (\PlatformSummary' {platformBranchName} -> platformBranchName) (\s@PlatformSummary' {} a -> s {platformBranchName = a} :: PlatformSummary)

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

-- | The ARN of the platform version.
platformSummary_platformArn :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformArn = Lens.lens (\PlatformSummary' {platformArn} -> platformArn) (\s@PlatformSummary' {} a -> s {platformArn = a} :: PlatformSummary)

-- | The state of the platform version\'s branch in its lifecycle.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
platformSummary_platformBranchLifecycleState :: Lens.Lens' PlatformSummary (Prelude.Maybe Prelude.Text)
platformSummary_platformBranchLifecycleState = Lens.lens (\PlatformSummary' {platformBranchLifecycleState} -> platformBranchLifecycleState) (\s@PlatformSummary' {} a -> s {platformBranchLifecycleState = a} :: PlatformSummary)

-- | The tiers in which the platform version runs.
platformSummary_supportedTierList :: Lens.Lens' PlatformSummary (Prelude.Maybe [Prelude.Text])
platformSummary_supportedTierList = Lens.lens (\PlatformSummary' {supportedTierList} -> supportedTierList) (\s@PlatformSummary' {} a -> s {supportedTierList = a} :: PlatformSummary) Prelude.. Lens.mapping Lens._Coerce

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

instance Core.FromXML PlatformSummary where
  parseXML x =
    PlatformSummary'
      Prelude.<$> (x Core..@? "OperatingSystemName")
      Prelude.<*> (x Core..@? "PlatformCategory")
      Prelude.<*> ( x Core..@? "SupportedAddonList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PlatformBranchName")
      Prelude.<*> (x Core..@? "PlatformOwner")
      Prelude.<*> (x Core..@? "PlatformStatus")
      Prelude.<*> (x Core..@? "PlatformVersion")
      Prelude.<*> (x Core..@? "PlatformArn")
      Prelude.<*> (x Core..@? "PlatformBranchLifecycleState")
      Prelude.<*> ( x Core..@? "SupportedTierList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PlatformLifecycleState")
      Prelude.<*> (x Core..@? "OperatingSystemVersion")

instance Prelude.Hashable PlatformSummary

instance Prelude.NFData PlatformSummary
