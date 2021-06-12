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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about a platform branch.
--
-- /See:/ 'newPlatformBranchSummary' smart constructor.
data PlatformBranchSummary = PlatformBranchSummary'
  { -- | The name of the platform branch.
    branchName :: Core.Maybe Core.Text,
    -- | An ordinal number that designates the order in which platform branches
    -- have been added to a platform. This can be helpful, for example, if your
    -- code calls the @ListPlatformBranches@ action and then displays a list of
    -- platform branches.
    --
    -- A larger @BranchOrder@ value designates a newer platform branch within
    -- the platform.
    branchOrder :: Core.Maybe Core.Int,
    -- | The support life cycle state of the platform branch.
    --
    -- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
    lifecycleState :: Core.Maybe Core.Text,
    -- | The environment tiers that platform versions in this branch support.
    --
    -- Possible values: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
    supportedTierList :: Core.Maybe [Core.Text],
    -- | The name of the platform to which this platform branch belongs.
    platformName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlatformBranchSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchName', 'platformBranchSummary_branchName' - The name of the platform branch.
--
-- 'branchOrder', 'platformBranchSummary_branchOrder' - An ordinal number that designates the order in which platform branches
-- have been added to a platform. This can be helpful, for example, if your
-- code calls the @ListPlatformBranches@ action and then displays a list of
-- platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within
-- the platform.
--
-- 'lifecycleState', 'platformBranchSummary_lifecycleState' - The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- 'supportedTierList', 'platformBranchSummary_supportedTierList' - The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
--
-- 'platformName', 'platformBranchSummary_platformName' - The name of the platform to which this platform branch belongs.
newPlatformBranchSummary ::
  PlatformBranchSummary
newPlatformBranchSummary =
  PlatformBranchSummary'
    { branchName = Core.Nothing,
      branchOrder = Core.Nothing,
      lifecycleState = Core.Nothing,
      supportedTierList = Core.Nothing,
      platformName = Core.Nothing
    }

-- | The name of the platform branch.
platformBranchSummary_branchName :: Lens.Lens' PlatformBranchSummary (Core.Maybe Core.Text)
platformBranchSummary_branchName = Lens.lens (\PlatformBranchSummary' {branchName} -> branchName) (\s@PlatformBranchSummary' {} a -> s {branchName = a} :: PlatformBranchSummary)

-- | An ordinal number that designates the order in which platform branches
-- have been added to a platform. This can be helpful, for example, if your
-- code calls the @ListPlatformBranches@ action and then displays a list of
-- platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within
-- the platform.
platformBranchSummary_branchOrder :: Lens.Lens' PlatformBranchSummary (Core.Maybe Core.Int)
platformBranchSummary_branchOrder = Lens.lens (\PlatformBranchSummary' {branchOrder} -> branchOrder) (\s@PlatformBranchSummary' {} a -> s {branchOrder = a} :: PlatformBranchSummary)

-- | The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
platformBranchSummary_lifecycleState :: Lens.Lens' PlatformBranchSummary (Core.Maybe Core.Text)
platformBranchSummary_lifecycleState = Lens.lens (\PlatformBranchSummary' {lifecycleState} -> lifecycleState) (\s@PlatformBranchSummary' {} a -> s {lifecycleState = a} :: PlatformBranchSummary)

-- | The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer\/Standard@ | @Worker\/SQS\/HTTP@
platformBranchSummary_supportedTierList :: Lens.Lens' PlatformBranchSummary (Core.Maybe [Core.Text])
platformBranchSummary_supportedTierList = Lens.lens (\PlatformBranchSummary' {supportedTierList} -> supportedTierList) (\s@PlatformBranchSummary' {} a -> s {supportedTierList = a} :: PlatformBranchSummary) Core.. Lens.mapping Lens._Coerce

-- | The name of the platform to which this platform branch belongs.
platformBranchSummary_platformName :: Lens.Lens' PlatformBranchSummary (Core.Maybe Core.Text)
platformBranchSummary_platformName = Lens.lens (\PlatformBranchSummary' {platformName} -> platformName) (\s@PlatformBranchSummary' {} a -> s {platformName = a} :: PlatformBranchSummary)

instance Core.FromXML PlatformBranchSummary where
  parseXML x =
    PlatformBranchSummary'
      Core.<$> (x Core..@? "BranchName")
      Core.<*> (x Core..@? "BranchOrder")
      Core.<*> (x Core..@? "LifecycleState")
      Core.<*> ( x Core..@? "SupportedTierList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "PlatformName")

instance Core.Hashable PlatformBranchSummary

instance Core.NFData PlatformBranchSummary
