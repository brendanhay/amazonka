{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionVersion
  ( OptionVersion (..)
  -- * Smart constructor
  , mkOptionVersion
  -- * Lenses
  , ovIsDefault
  , ovVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The version for an option. Option group option versions are returned by the @DescribeOptionGroupOptions@ action.
--
-- /See:/ 'mkOptionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { isDefault :: Core.Maybe Core.Bool
    -- ^ True if the version is the default version of the option, and otherwise false.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionVersion' value with any optional fields omitted.
mkOptionVersion
    :: OptionVersion
mkOptionVersion
  = OptionVersion'{isDefault = Core.Nothing, version = Core.Nothing}

-- | True if the version is the default version of the option, and otherwise false.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovIsDefault :: Lens.Lens' OptionVersion (Core.Maybe Core.Bool)
ovIsDefault = Lens.field @"isDefault"
{-# INLINEABLE ovIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The version of the option.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovVersion :: Lens.Lens' OptionVersion (Core.Maybe Core.Text)
ovVersion = Lens.field @"version"
{-# INLINEABLE ovVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromXML OptionVersion where
        parseXML x
          = OptionVersion' Core.<$>
              (x Core..@? "IsDefault") Core.<*> x Core..@? "Version"
