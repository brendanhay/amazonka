{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ImageConfig
  ( ImageConfig (..)
  -- * Smart constructor
  , mkImageConfig
  -- * Lenses
  , icRepositoryAccessMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.RepositoryAccessMode as Types

-- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
--
-- /See:/ 'mkImageConfig' smart constructor.
newtype ImageConfig = ImageConfig'
  { repositoryAccessMode :: Types.RepositoryAccessMode
    -- ^ Set this to one of the following values:
--
--
--     * @Platform@ - The model image is hosted in Amazon ECR.
--
--
--     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ImageConfig' value with any optional fields omitted.
mkImageConfig
    :: Types.RepositoryAccessMode -- ^ 'repositoryAccessMode'
    -> ImageConfig
mkImageConfig repositoryAccessMode
  = ImageConfig'{repositoryAccessMode}

-- | Set this to one of the following values:
--
--
--     * @Platform@ - The model image is hosted in Amazon ECR.
--
--
--     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
--
--
--
-- /Note:/ Consider using 'repositoryAccessMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRepositoryAccessMode :: Lens.Lens' ImageConfig Types.RepositoryAccessMode
icRepositoryAccessMode = Lens.field @"repositoryAccessMode"
{-# INLINEABLE icRepositoryAccessMode #-}
{-# DEPRECATED repositoryAccessMode "Use generic-lens or generic-optics with 'repositoryAccessMode' instead"  #-}

instance Core.FromJSON ImageConfig where
        toJSON ImageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RepositoryAccessMode" Core..= repositoryAccessMode)])

instance Core.FromJSON ImageConfig where
        parseJSON
          = Core.withObject "ImageConfig" Core.$
              \ x -> ImageConfig' Core.<$> (x Core..: "RepositoryAccessMode")
