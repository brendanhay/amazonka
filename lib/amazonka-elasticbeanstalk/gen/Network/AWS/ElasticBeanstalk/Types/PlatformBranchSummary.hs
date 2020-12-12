{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
  ( PlatformBranchSummary (..),

    -- * Smart constructor
    mkPlatformBranchSummary,

    -- * Lenses
    pbsBranchName,
    pbsBranchOrder,
    pbsPlatformName,
    pbsSupportedTierList,
    pbsLifecycleState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about a platform branch.
--
-- /See:/ 'mkPlatformBranchSummary' smart constructor.
data PlatformBranchSummary = PlatformBranchSummary'
  { branchName ::
      Lude.Maybe Lude.Text,
    branchOrder :: Lude.Maybe Lude.Int,
    platformName :: Lude.Maybe Lude.Text,
    supportedTierList :: Lude.Maybe [Lude.Text],
    lifecycleState :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformBranchSummary' with the minimum fields required to make a request.
--
-- * 'branchName' - The name of the platform branch.
-- * 'branchOrder' - An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within the platform.
-- * 'lifecycleState' - The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
-- * 'platformName' - The name of the platform to which this platform branch belongs.
-- * 'supportedTierList' - The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@
mkPlatformBranchSummary ::
  PlatformBranchSummary
mkPlatformBranchSummary =
  PlatformBranchSummary'
    { branchName = Lude.Nothing,
      branchOrder = Lude.Nothing,
      platformName = Lude.Nothing,
      supportedTierList = Lude.Nothing,
      lifecycleState = Lude.Nothing
    }

-- | The name of the platform branch.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsBranchName :: Lens.Lens' PlatformBranchSummary (Lude.Maybe Lude.Text)
pbsBranchName = Lens.lens (branchName :: PlatformBranchSummary -> Lude.Maybe Lude.Text) (\s a -> s {branchName = a} :: PlatformBranchSummary)
{-# DEPRECATED pbsBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within the platform.
--
-- /Note:/ Consider using 'branchOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsBranchOrder :: Lens.Lens' PlatformBranchSummary (Lude.Maybe Lude.Int)
pbsBranchOrder = Lens.lens (branchOrder :: PlatformBranchSummary -> Lude.Maybe Lude.Int) (\s a -> s {branchOrder = a} :: PlatformBranchSummary)
{-# DEPRECATED pbsBranchOrder "Use generic-lens or generic-optics with 'branchOrder' instead." #-}

-- | The name of the platform to which this platform branch belongs.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsPlatformName :: Lens.Lens' PlatformBranchSummary (Lude.Maybe Lude.Text)
pbsPlatformName = Lens.lens (platformName :: PlatformBranchSummary -> Lude.Maybe Lude.Text) (\s a -> s {platformName = a} :: PlatformBranchSummary)
{-# DEPRECATED pbsPlatformName "Use generic-lens or generic-optics with 'platformName' instead." #-}

-- | The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@
--
-- /Note:/ Consider using 'supportedTierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsSupportedTierList :: Lens.Lens' PlatformBranchSummary (Lude.Maybe [Lude.Text])
pbsSupportedTierList = Lens.lens (supportedTierList :: PlatformBranchSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedTierList = a} :: PlatformBranchSummary)
{-# DEPRECATED pbsSupportedTierList "Use generic-lens or generic-optics with 'supportedTierList' instead." #-}

-- | The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsLifecycleState :: Lens.Lens' PlatformBranchSummary (Lude.Maybe Lude.Text)
pbsLifecycleState = Lens.lens (lifecycleState :: PlatformBranchSummary -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleState = a} :: PlatformBranchSummary)
{-# DEPRECATED pbsLifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead." #-}

instance Lude.FromXML PlatformBranchSummary where
  parseXML x =
    PlatformBranchSummary'
      Lude.<$> (x Lude..@? "BranchName")
      Lude.<*> (x Lude..@? "BranchOrder")
      Lude.<*> (x Lude..@? "PlatformName")
      Lude.<*> ( x Lude..@? "SupportedTierList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "LifecycleState")
