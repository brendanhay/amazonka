{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceState
  ( InstanceState (..)
  -- * Smart constructor
  , mkInstanceState
  -- * Lenses
  , isfCode
  , isfName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the virtual private server (or /instance/ ) status.
--
-- /See:/ 'mkInstanceState' smart constructor.
data InstanceState = InstanceState'
  { code :: Core.Maybe Core.Int
    -- ^ The status code for the instance.
  , name :: Core.Maybe Core.Text
    -- ^ The state of the instance (e.g., @running@ or @pending@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceState' value with any optional fields omitted.
mkInstanceState
    :: InstanceState
mkInstanceState
  = InstanceState'{code = Core.Nothing, name = Core.Nothing}

-- | The status code for the instance.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfCode :: Lens.Lens' InstanceState (Core.Maybe Core.Int)
isfCode = Lens.field @"code"
{-# INLINEABLE isfCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The state of the instance (e.g., @running@ or @pending@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfName :: Lens.Lens' InstanceState (Core.Maybe Core.Text)
isfName = Lens.field @"name"
{-# INLINEABLE isfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON InstanceState where
        parseJSON
          = Core.withObject "InstanceState" Core.$
              \ x ->
                InstanceState' Core.<$>
                  (x Core..:? "code") Core.<*> x Core..:? "name"
