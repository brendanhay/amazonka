{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
  ( ApplicationDependencySummary (..),

    -- * Smart constructor
    mkApplicationDependencySummary,

    -- * Lenses
    adsApplicationId,
    adsSemanticVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A nested application summary.
--
-- /See:/ 'mkApplicationDependencySummary' smart constructor.
data ApplicationDependencySummary = ApplicationDependencySummary'
  { applicationId ::
      Lude.Text,
    semanticVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationDependencySummary' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the nested application.
-- * 'semanticVersion' - The semantic version of the nested application.
mkApplicationDependencySummary ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'semanticVersion'
  Lude.Text ->
  ApplicationDependencySummary
mkApplicationDependencySummary pApplicationId_ pSemanticVersion_ =
  ApplicationDependencySummary'
    { applicationId = pApplicationId_,
      semanticVersion = pSemanticVersion_
    }

-- | The Amazon Resource Name (ARN) of the nested application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsApplicationId :: Lens.Lens' ApplicationDependencySummary Lude.Text
adsApplicationId = Lens.lens (applicationId :: ApplicationDependencySummary -> Lude.Text) (\s a -> s {applicationId = a} :: ApplicationDependencySummary)
{-# DEPRECATED adsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the nested application.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsSemanticVersion :: Lens.Lens' ApplicationDependencySummary Lude.Text
adsSemanticVersion = Lens.lens (semanticVersion :: ApplicationDependencySummary -> Lude.Text) (\s a -> s {semanticVersion = a} :: ApplicationDependencySummary)
{-# DEPRECATED adsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

instance Lude.FromJSON ApplicationDependencySummary where
  parseJSON =
    Lude.withObject
      "ApplicationDependencySummary"
      ( \x ->
          ApplicationDependencySummary'
            Lude.<$> (x Lude..: "applicationId") Lude.<*> (x Lude..: "semanticVersion")
      )
