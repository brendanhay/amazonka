-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorResourcesSummary
  ( TrustedAdvisorResourcesSummary (..),

    -- * Smart constructor
    mkTrustedAdvisorResourcesSummary,

    -- * Lenses
    tarsResourcesProcessed,
    tarsResourcesFlagged,
    tarsResourcesIgnored,
    tarsResourcesSuppressed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about AWS resources that were analyzed in a call to Trusted Advisor 'DescribeTrustedAdvisorCheckSummaries' .
--
-- /See:/ 'mkTrustedAdvisorResourcesSummary' smart constructor.
data TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary'
  { resourcesProcessed ::
      Lude.Integer,
    resourcesFlagged ::
      Lude.Integer,
    resourcesIgnored ::
      Lude.Integer,
    resourcesSuppressed ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedAdvisorResourcesSummary' with the minimum fields required to make a request.
--
-- * 'resourcesFlagged' - The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
-- * 'resourcesIgnored' - The number of AWS resources ignored by Trusted Advisor because information was unavailable.
-- * 'resourcesProcessed' - The number of AWS resources that were analyzed by the Trusted Advisor check.
-- * 'resourcesSuppressed' - The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
mkTrustedAdvisorResourcesSummary ::
  -- | 'resourcesProcessed'
  Lude.Integer ->
  -- | 'resourcesFlagged'
  Lude.Integer ->
  -- | 'resourcesIgnored'
  Lude.Integer ->
  -- | 'resourcesSuppressed'
  Lude.Integer ->
  TrustedAdvisorResourcesSummary
mkTrustedAdvisorResourcesSummary
  pResourcesProcessed_
  pResourcesFlagged_
  pResourcesIgnored_
  pResourcesSuppressed_ =
    TrustedAdvisorResourcesSummary'
      { resourcesProcessed =
          pResourcesProcessed_,
        resourcesFlagged = pResourcesFlagged_,
        resourcesIgnored = pResourcesIgnored_,
        resourcesSuppressed = pResourcesSuppressed_
      }

-- | The number of AWS resources that were analyzed by the Trusted Advisor check.
--
-- /Note:/ Consider using 'resourcesProcessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesProcessed :: Lens.Lens' TrustedAdvisorResourcesSummary Lude.Integer
tarsResourcesProcessed = Lens.lens (resourcesProcessed :: TrustedAdvisorResourcesSummary -> Lude.Integer) (\s a -> s {resourcesProcessed = a} :: TrustedAdvisorResourcesSummary)
{-# DEPRECATED tarsResourcesProcessed "Use generic-lens or generic-optics with 'resourcesProcessed' instead." #-}

-- | The number of AWS resources that were flagged (listed) by the Trusted Advisor check.
--
-- /Note:/ Consider using 'resourcesFlagged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesFlagged :: Lens.Lens' TrustedAdvisorResourcesSummary Lude.Integer
tarsResourcesFlagged = Lens.lens (resourcesFlagged :: TrustedAdvisorResourcesSummary -> Lude.Integer) (\s a -> s {resourcesFlagged = a} :: TrustedAdvisorResourcesSummary)
{-# DEPRECATED tarsResourcesFlagged "Use generic-lens or generic-optics with 'resourcesFlagged' instead." #-}

-- | The number of AWS resources ignored by Trusted Advisor because information was unavailable.
--
-- /Note:/ Consider using 'resourcesIgnored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesIgnored :: Lens.Lens' TrustedAdvisorResourcesSummary Lude.Integer
tarsResourcesIgnored = Lens.lens (resourcesIgnored :: TrustedAdvisorResourcesSummary -> Lude.Integer) (\s a -> s {resourcesIgnored = a} :: TrustedAdvisorResourcesSummary)
{-# DEPRECATED tarsResourcesIgnored "Use generic-lens or generic-optics with 'resourcesIgnored' instead." #-}

-- | The number of AWS resources ignored by Trusted Advisor because they were marked as suppressed by the user.
--
-- /Note:/ Consider using 'resourcesSuppressed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tarsResourcesSuppressed :: Lens.Lens' TrustedAdvisorResourcesSummary Lude.Integer
tarsResourcesSuppressed = Lens.lens (resourcesSuppressed :: TrustedAdvisorResourcesSummary -> Lude.Integer) (\s a -> s {resourcesSuppressed = a} :: TrustedAdvisorResourcesSummary)
{-# DEPRECATED tarsResourcesSuppressed "Use generic-lens or generic-optics with 'resourcesSuppressed' instead." #-}

instance Lude.FromJSON TrustedAdvisorResourcesSummary where
  parseJSON =
    Lude.withObject
      "TrustedAdvisorResourcesSummary"
      ( \x ->
          TrustedAdvisorResourcesSummary'
            Lude.<$> (x Lude..: "resourcesProcessed")
            Lude.<*> (x Lude..: "resourcesFlagged")
            Lude.<*> (x Lude..: "resourcesIgnored")
            Lude.<*> (x Lude..: "resourcesSuppressed")
      )
