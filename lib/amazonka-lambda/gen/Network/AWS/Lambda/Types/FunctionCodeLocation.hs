{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.FunctionCodeLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.FunctionCodeLocation
  ( FunctionCodeLocation (..)
  -- * Smart constructor
  , mkFunctionCodeLocation
  -- * Lenses
  , fclLocation
  , fclRepositoryType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a function's deployment package.
--
-- /See:/ 'mkFunctionCodeLocation' smart constructor.
data FunctionCodeLocation = FunctionCodeLocation'
  { location :: Core.Maybe Core.Text
    -- ^ A presigned URL that you can use to download the deployment package.
  , repositoryType :: Core.Maybe Core.Text
    -- ^ The service that's hosting the file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionCodeLocation' value with any optional fields omitted.
mkFunctionCodeLocation
    :: FunctionCodeLocation
mkFunctionCodeLocation
  = FunctionCodeLocation'{location = Core.Nothing,
                          repositoryType = Core.Nothing}

-- | A presigned URL that you can use to download the deployment package.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fclLocation :: Lens.Lens' FunctionCodeLocation (Core.Maybe Core.Text)
fclLocation = Lens.field @"location"
{-# INLINEABLE fclLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The service that's hosting the file.
--
-- /Note:/ Consider using 'repositoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fclRepositoryType :: Lens.Lens' FunctionCodeLocation (Core.Maybe Core.Text)
fclRepositoryType = Lens.field @"repositoryType"
{-# INLINEABLE fclRepositoryType #-}
{-# DEPRECATED repositoryType "Use generic-lens or generic-optics with 'repositoryType' instead"  #-}

instance Core.FromJSON FunctionCodeLocation where
        parseJSON
          = Core.withObject "FunctionCodeLocation" Core.$
              \ x ->
                FunctionCodeLocation' Core.<$>
                  (x Core..:? "Location") Core.<*> x Core..:? "RepositoryType"
