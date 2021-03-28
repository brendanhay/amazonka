{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Authentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.Authentication
  ( Authentication (..)
  -- * Smart constructor
  , mkAuthentication
  -- * Lenses
  , aPasswordCount
  , aType
  ) where

import qualified Network.AWS.ElastiCache.Types.AuthenticationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether the user requires a password to authenticate.
--
-- /See:/ 'mkAuthentication' smart constructor.
data Authentication = Authentication'
  { passwordCount :: Core.Maybe Core.Int
    -- ^ The number of passwords belonging to the user. The maximum is two.
  , type' :: Core.Maybe Types.AuthenticationType
    -- ^ Indicates whether the user requires a password to authenticate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Authentication' value with any optional fields omitted.
mkAuthentication
    :: Authentication
mkAuthentication
  = Authentication'{passwordCount = Core.Nothing,
                    type' = Core.Nothing}

-- | The number of passwords belonging to the user. The maximum is two.
--
-- /Note:/ Consider using 'passwordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPasswordCount :: Lens.Lens' Authentication (Core.Maybe Core.Int)
aPasswordCount = Lens.field @"passwordCount"
{-# INLINEABLE aPasswordCount #-}
{-# DEPRECATED passwordCount "Use generic-lens or generic-optics with 'passwordCount' instead"  #-}

-- | Indicates whether the user requires a password to authenticate.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Authentication (Core.Maybe Types.AuthenticationType)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML Authentication where
        parseXML x
          = Authentication' Core.<$>
              (x Core..@? "PasswordCount") Core.<*> x Core..@? "Type"
