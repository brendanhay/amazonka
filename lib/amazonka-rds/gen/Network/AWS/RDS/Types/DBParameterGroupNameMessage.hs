{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBParameterGroupNameMessage
  ( DBParameterGroupNameMessage (..)
  -- * Smart constructor
  , mkDBParameterGroupNameMessage
  -- * Lenses
  , dbpgnmDBParameterGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the result of a successful invocation of the @ModifyDBParameterGroup@ or @ResetDBParameterGroup@ action. 
--
-- /See:/ 'mkDBParameterGroupNameMessage' smart constructor.
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'
  { dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DBParameterGroupNameMessage' value with any optional fields omitted.
mkDBParameterGroupNameMessage
    :: DBParameterGroupNameMessage
mkDBParameterGroupNameMessage
  = DBParameterGroupNameMessage'{dBParameterGroupName = Core.Nothing}

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgnmDBParameterGroupName :: Lens.Lens' DBParameterGroupNameMessage (Core.Maybe Core.Text)
dbpgnmDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE dbpgnmDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

instance Core.FromXML DBParameterGroupNameMessage where
        parseXML x
          = DBParameterGroupNameMessage' Core.<$>
              (x Core..@? "DBParameterGroupName")
