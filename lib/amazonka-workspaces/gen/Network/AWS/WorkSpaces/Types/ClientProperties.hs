{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ClientProperties
  ( ClientProperties (..)
  -- * Smart constructor
  , mkClientProperties
  -- * Lenses
  , cpReconnectEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ReconnectEnum as Types

-- | Describes an Amazon WorkSpaces client.
--
-- /See:/ 'mkClientProperties' smart constructor.
newtype ClientProperties = ClientProperties'
  { reconnectEnabled :: Core.Maybe Types.ReconnectEnum
    -- ^ Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ClientProperties' value with any optional fields omitted.
mkClientProperties
    :: ClientProperties
mkClientProperties
  = ClientProperties'{reconnectEnabled = Core.Nothing}

-- | Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials. 
--
-- /Note:/ Consider using 'reconnectEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpReconnectEnabled :: Lens.Lens' ClientProperties (Core.Maybe Types.ReconnectEnum)
cpReconnectEnabled = Lens.field @"reconnectEnabled"
{-# INLINEABLE cpReconnectEnabled #-}
{-# DEPRECATED reconnectEnabled "Use generic-lens or generic-optics with 'reconnectEnabled' instead"  #-}

instance Core.FromJSON ClientProperties where
        toJSON ClientProperties{..}
          = Core.object
              (Core.catMaybes
                 [("ReconnectEnabled" Core..=) Core.<$> reconnectEnabled])

instance Core.FromJSON ClientProperties where
        parseJSON
          = Core.withObject "ClientProperties" Core.$
              \ x -> ClientProperties' Core.<$> (x Core..:? "ReconnectEnabled")
