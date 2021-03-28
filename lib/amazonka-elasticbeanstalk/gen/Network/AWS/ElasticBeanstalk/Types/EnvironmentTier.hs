{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
  ( EnvironmentTier (..)
  -- * Smart constructor
  , mkEnvironmentTier
  -- * Lenses
  , etName
  , etType
  , etVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the properties of an environment tier
--
-- /See:/ 'mkEnvironmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
  { name :: Core.Maybe Core.Text
    -- ^ The name of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @WebServer@ 
--
--
--     * For /Worker tier/ – @Worker@ 
--
--
  , type' :: Core.Maybe Core.Text
    -- ^ The type of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @Standard@ 
--
--
--     * For /Worker tier/ – @SQS/HTTP@ 
--
--
  , version :: Core.Maybe Core.Text
    -- ^ The version of this environment tier. When you don't set a value to it, Elastic Beanstalk uses the latest compatible worker tier version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentTier' value with any optional fields omitted.
mkEnvironmentTier
    :: EnvironmentTier
mkEnvironmentTier
  = EnvironmentTier'{name = Core.Nothing, type' = Core.Nothing,
                     version = Core.Nothing}

-- | The name of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @WebServer@ 
--
--
--     * For /Worker tier/ – @Worker@ 
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etName :: Lens.Lens' EnvironmentTier (Core.Maybe Core.Text)
etName = Lens.field @"name"
{-# INLINEABLE etName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of this environment tier.
--
-- Valid values:
--
--     * For /Web server tier/ – @Standard@ 
--
--
--     * For /Worker tier/ – @SQS/HTTP@ 
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etType :: Lens.Lens' EnvironmentTier (Core.Maybe Core.Text)
etType = Lens.field @"type'"
{-# INLINEABLE etType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The version of this environment tier. When you don't set a value to it, Elastic Beanstalk uses the latest compatible worker tier version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etVersion :: Lens.Lens' EnvironmentTier (Core.Maybe Core.Text)
etVersion = Lens.field @"version"
{-# INLINEABLE etVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.ToQuery EnvironmentTier where
        toQuery EnvironmentTier{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Name") name Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Type") type'
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Version") version

instance Core.FromXML EnvironmentTier where
        parseXML x
          = EnvironmentTier' Core.<$>
              (x Core..@? "Name") Core.<*> x Core..@? "Type" Core.<*>
                x Core..@? "Version"
