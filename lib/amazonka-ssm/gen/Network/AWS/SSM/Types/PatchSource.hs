{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchSource
  ( PatchSource (..)
  -- * Smart constructor
  , mkPatchSource
  -- * Lenses
  , psName
  , psProducts
  , psConfiguration
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Configuration as Types
import qualified Network.AWS.SSM.Types.Name as Types
import qualified Network.AWS.SSM.Types.PatchSourceProduct as Types

-- | Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.
--
-- /See:/ 'mkPatchSource' smart constructor.
data PatchSource = PatchSource'
  { name :: Types.Name
    -- ^ The name specified to identify the patch source.
  , products :: Core.NonEmpty Types.PatchSourceProduct
    -- ^ The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
  , configuration :: Types.Configuration
    -- ^ The value of the yum repo configuration. For example:
--
-- @[main]@ 
-- @cachedir=/var/cache/yum/$basesearch$releasever@ 
-- @keepcache=0@ 
-- @debuglevel=2@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchSource' value with any optional fields omitted.
mkPatchSource
    :: Types.Name -- ^ 'name'
    -> Core.NonEmpty Types.PatchSourceProduct -- ^ 'products'
    -> Types.Configuration -- ^ 'configuration'
    -> PatchSource
mkPatchSource name products configuration
  = PatchSource'{name, products, configuration}

-- | The name specified to identify the patch source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PatchSource Types.Name
psName = Lens.field @"name"
{-# INLINEABLE psName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
--
-- /Note:/ Consider using 'products' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProducts :: Lens.Lens' PatchSource (Core.NonEmpty Types.PatchSourceProduct)
psProducts = Lens.field @"products"
{-# INLINEABLE psProducts #-}
{-# DEPRECATED products "Use generic-lens or generic-optics with 'products' instead"  #-}

-- | The value of the yum repo configuration. For example:
--
-- @[main]@ 
-- @cachedir=/var/cache/yum/$basesearch$releasever@ 
-- @keepcache=0@ 
-- @debuglevel=2@ 
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psConfiguration :: Lens.Lens' PatchSource Types.Configuration
psConfiguration = Lens.field @"configuration"
{-# INLINEABLE psConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

instance Core.FromJSON PatchSource where
        toJSON PatchSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Products" Core..= products),
                  Core.Just ("Configuration" Core..= configuration)])

instance Core.FromJSON PatchSource where
        parseJSON
          = Core.withObject "PatchSource" Core.$
              \ x ->
                PatchSource' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Products" Core.<*>
                    x Core..: "Configuration"
