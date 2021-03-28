{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TlsContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TlsContext
  ( TlsContext (..)
  -- * Smart constructor
  , mkTlsContext
  -- * Lenses
  , tcServerName
  ) where

import qualified Network.AWS.IoT.Types.ServerName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the TLS context to use for the test authorizer request.
--
-- /See:/ 'mkTlsContext' smart constructor.
newtype TlsContext = TlsContext'
  { serverName :: Core.Maybe Types.ServerName
    -- ^ The value of the @serverName@ key in a TLS authorization request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TlsContext' value with any optional fields omitted.
mkTlsContext
    :: TlsContext
mkTlsContext = TlsContext'{serverName = Core.Nothing}

-- | The value of the @serverName@ key in a TLS authorization request.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcServerName :: Lens.Lens' TlsContext (Core.Maybe Types.ServerName)
tcServerName = Lens.field @"serverName"
{-# INLINEABLE tcServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

instance Core.FromJSON TlsContext where
        toJSON TlsContext{..}
          = Core.object
              (Core.catMaybes [("serverName" Core..=) Core.<$> serverName])
