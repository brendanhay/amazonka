{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckSummary
  ( TrustedAdvisorCheckSummary (..),

    -- * Smart constructor
    mkTrustedAdvisorCheckSummary,

    -- * Lenses
    tacsCategorySpecificSummary,
    tacsStatus,
    tacsCheckId,
    tacsResourcesSummary,
    tacsTimestamp,
    tacsHasFlaggedResources,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
import Network.AWS.Support.Types.TrustedAdvisorResourcesSummary

-- | A summary of a Trusted Advisor check result, including the alert status, last refresh, and number of resources examined.
--
-- /See:/ 'mkTrustedAdvisorCheckSummary' smart constructor.
data TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary'
  { -- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
    categorySpecificSummary :: TrustedAdvisorCategorySpecificSummary,
    -- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
    status :: Lude.Text,
    -- | The unique identifier for the Trusted Advisor check.
    checkId :: Lude.Text,
    resourcesSummary :: TrustedAdvisorResourcesSummary,
    -- | The time of the last refresh of the check.
    timestamp :: Lude.Text,
    -- | Specifies whether the Trusted Advisor check has flagged resources.
    hasFlaggedResources :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrustedAdvisorCheckSummary' with the minimum fields required to make a request.
--
-- * 'categorySpecificSummary' - Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
-- * 'status' - The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
-- * 'checkId' - The unique identifier for the Trusted Advisor check.
-- * 'resourcesSummary' -
-- * 'timestamp' - The time of the last refresh of the check.
-- * 'hasFlaggedResources' - Specifies whether the Trusted Advisor check has flagged resources.
mkTrustedAdvisorCheckSummary ::
  -- | 'categorySpecificSummary'
  TrustedAdvisorCategorySpecificSummary ->
  -- | 'status'
  Lude.Text ->
  -- | 'checkId'
  Lude.Text ->
  -- | 'resourcesSummary'
  TrustedAdvisorResourcesSummary ->
  -- | 'timestamp'
  Lude.Text ->
  TrustedAdvisorCheckSummary
mkTrustedAdvisorCheckSummary
  pCategorySpecificSummary_
  pStatus_
  pCheckId_
  pResourcesSummary_
  pTimestamp_ =
    TrustedAdvisorCheckSummary'
      { categorySpecificSummary =
          pCategorySpecificSummary_,
        status = pStatus_,
        checkId = pCheckId_,
        resourcesSummary = pResourcesSummary_,
        timestamp = pTimestamp_,
        hasFlaggedResources = Lude.Nothing
      }

-- | Summary information that relates to the category of the check. Cost Optimizing is the only category that is currently supported.
--
-- /Note:/ Consider using 'categorySpecificSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsCategorySpecificSummary :: Lens.Lens' TrustedAdvisorCheckSummary TrustedAdvisorCategorySpecificSummary
tacsCategorySpecificSummary = Lens.lens (categorySpecificSummary :: TrustedAdvisorCheckSummary -> TrustedAdvisorCategorySpecificSummary) (\s a -> s {categorySpecificSummary = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsCategorySpecificSummary "Use generic-lens or generic-optics with 'categorySpecificSummary' instead." #-}

-- | The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsStatus :: Lens.Lens' TrustedAdvisorCheckSummary Lude.Text
tacsStatus = Lens.lens (status :: TrustedAdvisorCheckSummary -> Lude.Text) (\s a -> s {status = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsCheckId :: Lens.Lens' TrustedAdvisorCheckSummary Lude.Text
tacsCheckId = Lens.lens (checkId :: TrustedAdvisorCheckSummary -> Lude.Text) (\s a -> s {checkId = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourcesSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsResourcesSummary :: Lens.Lens' TrustedAdvisorCheckSummary TrustedAdvisorResourcesSummary
tacsResourcesSummary = Lens.lens (resourcesSummary :: TrustedAdvisorCheckSummary -> TrustedAdvisorResourcesSummary) (\s a -> s {resourcesSummary = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsResourcesSummary "Use generic-lens or generic-optics with 'resourcesSummary' instead." #-}

-- | The time of the last refresh of the check.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsTimestamp :: Lens.Lens' TrustedAdvisorCheckSummary Lude.Text
tacsTimestamp = Lens.lens (timestamp :: TrustedAdvisorCheckSummary -> Lude.Text) (\s a -> s {timestamp = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | Specifies whether the Trusted Advisor check has flagged resources.
--
-- /Note:/ Consider using 'hasFlaggedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacsHasFlaggedResources :: Lens.Lens' TrustedAdvisorCheckSummary (Lude.Maybe Lude.Bool)
tacsHasFlaggedResources = Lens.lens (hasFlaggedResources :: TrustedAdvisorCheckSummary -> Lude.Maybe Lude.Bool) (\s a -> s {hasFlaggedResources = a} :: TrustedAdvisorCheckSummary)
{-# DEPRECATED tacsHasFlaggedResources "Use generic-lens or generic-optics with 'hasFlaggedResources' instead." #-}

instance Lude.FromJSON TrustedAdvisorCheckSummary where
  parseJSON =
    Lude.withObject
      "TrustedAdvisorCheckSummary"
      ( \x ->
          TrustedAdvisorCheckSummary'
            Lude.<$> (x Lude..: "categorySpecificSummary")
            Lude.<*> (x Lude..: "status")
            Lude.<*> (x Lude..: "checkId")
            Lude.<*> (x Lude..: "resourcesSummary")
            Lude.<*> (x Lude..: "timestamp")
            Lude.<*> (x Lude..:? "hasFlaggedResources")
      )
