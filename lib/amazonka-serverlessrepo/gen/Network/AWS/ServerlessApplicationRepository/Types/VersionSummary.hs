{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
  ( VersionSummary (..)
  -- * Smart constructor
  , mkVersionSummary
  -- * Lenses
  , vsCreationTime
  , vsApplicationId
  , vsSemanticVersion
  , vsSourceCodeUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An application version summary.
--
-- /See:/ 'mkVersionSummary' smart constructor.
data VersionSummary = VersionSummary'
  { creationTime :: Core.Text
    -- ^ The date and time this resource was created.
  , applicationId :: Core.Text
    -- ^ The application Amazon Resource Name (ARN).
  , semanticVersion :: Core.Text
    -- ^ The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
  , sourceCodeUrl :: Core.Maybe Core.Text
    -- ^ A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VersionSummary' value with any optional fields omitted.
mkVersionSummary
    :: Core.Text -- ^ 'creationTime'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'semanticVersion'
    -> VersionSummary
mkVersionSummary creationTime applicationId semanticVersion
  = VersionSummary'{creationTime, applicationId, semanticVersion,
                    sourceCodeUrl = Core.Nothing}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsCreationTime :: Lens.Lens' VersionSummary Core.Text
vsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE vsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsApplicationId :: Lens.Lens' VersionSummary Core.Text
vsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE vsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSemanticVersion :: Lens.Lens' VersionSummary Core.Text
vsSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE vsSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSourceCodeUrl :: Lens.Lens' VersionSummary (Core.Maybe Core.Text)
vsSourceCodeUrl = Lens.field @"sourceCodeUrl"
{-# INLINEABLE vsSourceCodeUrl #-}
{-# DEPRECATED sourceCodeUrl "Use generic-lens or generic-optics with 'sourceCodeUrl' instead"  #-}

instance Core.FromJSON VersionSummary where
        parseJSON
          = Core.withObject "VersionSummary" Core.$
              \ x ->
                VersionSummary' Core.<$>
                  (x Core..: "creationTime") Core.<*> x Core..: "applicationId"
                    Core.<*> x Core..: "semanticVersion"
                    Core.<*> x Core..:? "sourceCodeUrl"
