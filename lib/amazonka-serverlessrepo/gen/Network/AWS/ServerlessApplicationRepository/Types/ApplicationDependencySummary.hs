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
import qualified Network.AWS.Prelude as Core

-- | A nested application summary.
--
-- /See:/ 'mkApplicationDependencySummary' smart constructor.
data ApplicationDependencySummary = ApplicationDependencySummary'
  { -- | The Amazon Resource Name (ARN) of the nested application.
    applicationId :: Core.Text,
    -- | The semantic version of the nested application.
    semanticVersion :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationDependencySummary' value with any optional fields omitted.
mkApplicationDependencySummary ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'semanticVersion'
  Core.Text ->
  ApplicationDependencySummary
mkApplicationDependencySummary applicationId semanticVersion =
  ApplicationDependencySummary' {applicationId, semanticVersion}

-- | The Amazon Resource Name (ARN) of the nested application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsApplicationId :: Lens.Lens' ApplicationDependencySummary Core.Text
adsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED adsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the nested application.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsSemanticVersion :: Lens.Lens' ApplicationDependencySummary Core.Text
adsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED adsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

instance Core.FromJSON ApplicationDependencySummary where
  parseJSON =
    Core.withObject "ApplicationDependencySummary" Core.$
      \x ->
        ApplicationDependencySummary'
          Core.<$> (x Core..: "applicationId") Core.<*> (x Core..: "semanticVersion")
