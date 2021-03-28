{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.RetentionPolicy
  ( RetentionPolicy (..)
  -- * Smart constructor
  , mkRetentionPolicy
  -- * Lenses
  , rpHomeEfsFileSystem
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.RetentionType as Types

-- | The retention policy for data stored on an Amazon Elastic File System (EFS) volume.
--
-- /See:/ 'mkRetentionPolicy' smart constructor.
newtype RetentionPolicy = RetentionPolicy'
  { homeEfsFileSystem :: Core.Maybe Types.RetentionType
    -- ^ The default is @Retain@ , which specifies to keep the data stored on the EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RetentionPolicy' value with any optional fields omitted.
mkRetentionPolicy
    :: RetentionPolicy
mkRetentionPolicy
  = RetentionPolicy'{homeEfsFileSystem = Core.Nothing}

-- | The default is @Retain@ , which specifies to keep the data stored on the EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
--
-- /Note:/ Consider using 'homeEfsFileSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpHomeEfsFileSystem :: Lens.Lens' RetentionPolicy (Core.Maybe Types.RetentionType)
rpHomeEfsFileSystem = Lens.field @"homeEfsFileSystem"
{-# INLINEABLE rpHomeEfsFileSystem #-}
{-# DEPRECATED homeEfsFileSystem "Use generic-lens or generic-optics with 'homeEfsFileSystem' instead"  #-}

instance Core.FromJSON RetentionPolicy where
        toJSON RetentionPolicy{..}
          = Core.object
              (Core.catMaybes
                 [("HomeEfsFileSystem" Core..=) Core.<$> homeEfsFileSystem])
