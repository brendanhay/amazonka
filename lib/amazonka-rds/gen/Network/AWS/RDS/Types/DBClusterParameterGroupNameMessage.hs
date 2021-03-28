{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
  ( DBClusterParameterGroupNameMessage (..)
  -- * Smart constructor
  , mkDBClusterParameterGroupNameMessage
  -- * Lenses
  , dbcpgnmDBClusterParameterGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | 
--
-- /See:/ 'mkDBClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { dBClusterParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterParameterGroupNameMessage' value with any optional fields omitted.
mkDBClusterParameterGroupNameMessage
    :: DBClusterParameterGroupNameMessage
mkDBClusterParameterGroupNameMessage
  = DBClusterParameterGroupNameMessage'{dBClusterParameterGroupName =
                                          Core.Nothing}

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must be 1 to 255 letters or numbers.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcpgnmDBClusterParameterGroupName :: Lens.Lens' DBClusterParameterGroupNameMessage (Core.Maybe Core.Text)
dbcpgnmDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE dbcpgnmDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

instance Core.FromXML DBClusterParameterGroupNameMessage where
        parseXML x
          = DBClusterParameterGroupNameMessage' Core.<$>
              (x Core..@? "DBClusterParameterGroupName")
