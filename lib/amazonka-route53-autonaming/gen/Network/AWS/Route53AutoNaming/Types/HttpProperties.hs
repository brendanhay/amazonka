{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HttpProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.HttpProperties
  ( HttpProperties (..)
  -- * Smart constructor
  , mkHttpProperties
  -- * Lenses
  , hpHttpName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.HttpName as Types

-- | A complex type that contains the name of an HTTP namespace.
--
-- /See:/ 'mkHttpProperties' smart constructor.
newtype HttpProperties = HttpProperties'
  { httpName :: Core.Maybe Types.HttpName
    -- ^ The name of an HTTP namespace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HttpProperties' value with any optional fields omitted.
mkHttpProperties
    :: HttpProperties
mkHttpProperties = HttpProperties'{httpName = Core.Nothing}

-- | The name of an HTTP namespace.
--
-- /Note:/ Consider using 'httpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpHttpName :: Lens.Lens' HttpProperties (Core.Maybe Types.HttpName)
hpHttpName = Lens.field @"httpName"
{-# INLINEABLE hpHttpName #-}
{-# DEPRECATED httpName "Use generic-lens or generic-optics with 'httpName' instead"  #-}

instance Core.FromJSON HttpProperties where
        parseJSON
          = Core.withObject "HttpProperties" Core.$
              \ x -> HttpProperties' Core.<$> (x Core..:? "HttpName")
