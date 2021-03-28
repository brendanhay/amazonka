{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ServiceId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.ServiceId
  ( ServiceId (..)
  -- * Smart constructor
  , mkServiceId
  -- * Lenses
  , siAccountId
  , siName
  , siNames
  , siType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | 
--
-- /See:/ 'mkServiceId' smart constructor.
data ServiceId = ServiceId'
  { accountId :: Core.Maybe Core.Text
    -- ^ 
  , name :: Core.Maybe Core.Text
    -- ^ 
  , names :: Core.Maybe [Core.Text]
    -- ^ 
  , type' :: Core.Maybe Core.Text
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceId' value with any optional fields omitted.
mkServiceId
    :: ServiceId
mkServiceId
  = ServiceId'{accountId = Core.Nothing, name = Core.Nothing,
               names = Core.Nothing, type' = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccountId :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
siAccountId = Lens.field @"accountId"
{-# INLINEABLE siAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siName :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
siName = Lens.field @"name"
{-# INLINEABLE siName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siNames :: Lens.Lens' ServiceId (Core.Maybe [Core.Text])
siNames = Lens.field @"names"
{-# INLINEABLE siNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siType :: Lens.Lens' ServiceId (Core.Maybe Core.Text)
siType = Lens.field @"type'"
{-# INLINEABLE siType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ServiceId where
        parseJSON
          = Core.withObject "ServiceId" Core.$
              \ x ->
                ServiceId' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "Names"
                    Core.<*> x Core..:? "Type"
