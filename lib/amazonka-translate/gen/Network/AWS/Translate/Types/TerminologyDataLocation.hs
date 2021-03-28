{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TerminologyDataLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.TerminologyDataLocation
  ( TerminologyDataLocation (..)
  -- * Smart constructor
  , mkTerminologyDataLocation
  -- * Lenses
  , tdlRepositoryType
  , tdlLocation
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location of the custom terminology data.
--
-- /See:/ 'mkTerminologyDataLocation' smart constructor.
data TerminologyDataLocation = TerminologyDataLocation'
  { repositoryType :: Core.Text
    -- ^ The repository type for the custom terminology data.
  , location :: Core.Text
    -- ^ The location of the custom terminology data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminologyDataLocation' value with any optional fields omitted.
mkTerminologyDataLocation
    :: Core.Text -- ^ 'repositoryType'
    -> Core.Text -- ^ 'location'
    -> TerminologyDataLocation
mkTerminologyDataLocation repositoryType location
  = TerminologyDataLocation'{repositoryType, location}

-- | The repository type for the custom terminology data.
--
-- /Note:/ Consider using 'repositoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdlRepositoryType :: Lens.Lens' TerminologyDataLocation Core.Text
tdlRepositoryType = Lens.field @"repositoryType"
{-# INLINEABLE tdlRepositoryType #-}
{-# DEPRECATED repositoryType "Use generic-lens or generic-optics with 'repositoryType' instead"  #-}

-- | The location of the custom terminology data.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdlLocation :: Lens.Lens' TerminologyDataLocation Core.Text
tdlLocation = Lens.field @"location"
{-# INLINEABLE tdlLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.FromJSON TerminologyDataLocation where
        parseJSON
          = Core.withObject "TerminologyDataLocation" Core.$
              \ x ->
                TerminologyDataLocation' Core.<$>
                  (x Core..: "RepositoryType") Core.<*> x Core..: "Location"
