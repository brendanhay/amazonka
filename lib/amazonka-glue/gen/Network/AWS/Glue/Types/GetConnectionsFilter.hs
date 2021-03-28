{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GetConnectionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.GetConnectionsFilter
  ( GetConnectionsFilter (..)
  -- * Smart constructor
  , mkGetConnectionsFilter
  -- * Lenses
  , gcfConnectionType
  , gcfMatchCriteria
  ) where

import qualified Network.AWS.Glue.Types.ConnectionType as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the connection definitions that are returned by the @GetConnections@ API operation.
--
-- /See:/ 'mkGetConnectionsFilter' smart constructor.
data GetConnectionsFilter = GetConnectionsFilter'
  { connectionType :: Core.Maybe Types.ConnectionType
    -- ^ The type of connections to return. Currently, SFTP is not supported.
  , matchCriteria :: Core.Maybe [Types.NameString]
    -- ^ A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConnectionsFilter' value with any optional fields omitted.
mkGetConnectionsFilter
    :: GetConnectionsFilter
mkGetConnectionsFilter
  = GetConnectionsFilter'{connectionType = Core.Nothing,
                          matchCriteria = Core.Nothing}

-- | The type of connections to return. Currently, SFTP is not supported.
--
-- /Note:/ Consider using 'connectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfConnectionType :: Lens.Lens' GetConnectionsFilter (Core.Maybe Types.ConnectionType)
gcfConnectionType = Lens.field @"connectionType"
{-# INLINEABLE gcfConnectionType #-}
{-# DEPRECATED connectionType "Use generic-lens or generic-optics with 'connectionType' instead"  #-}

-- | A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
--
-- /Note:/ Consider using 'matchCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfMatchCriteria :: Lens.Lens' GetConnectionsFilter (Core.Maybe [Types.NameString])
gcfMatchCriteria = Lens.field @"matchCriteria"
{-# INLINEABLE gcfMatchCriteria #-}
{-# DEPRECATED matchCriteria "Use generic-lens or generic-optics with 'matchCriteria' instead"  #-}

instance Core.FromJSON GetConnectionsFilter where
        toJSON GetConnectionsFilter{..}
          = Core.object
              (Core.catMaybes
                 [("ConnectionType" Core..=) Core.<$> connectionType,
                  ("MatchCriteria" Core..=) Core.<$> matchCriteria])
