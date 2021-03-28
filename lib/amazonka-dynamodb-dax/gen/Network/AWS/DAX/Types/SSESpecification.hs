{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSESpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.SSESpecification
  ( SSESpecification (..)
  -- * Smart constructor
  , mkSSESpecification
  -- * Lenses
  , ssesEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'mkSSESpecification' smart constructor.
newtype SSESpecification = SSESpecification'
  { enabled :: Core.Bool
    -- ^ Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SSESpecification' value with any optional fields omitted.
mkSSESpecification
    :: Core.Bool -- ^ 'enabled'
    -> SSESpecification
mkSSESpecification enabled = SSESpecification'{enabled}

-- | Indicates whether server-side encryption is enabled (true) or disabled (false) on the cluster.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesEnabled :: Lens.Lens' SSESpecification Core.Bool
ssesEnabled = Lens.field @"enabled"
{-# INLINEABLE ssesEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON SSESpecification where
        toJSON SSESpecification{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Enabled" Core..= enabled)])
