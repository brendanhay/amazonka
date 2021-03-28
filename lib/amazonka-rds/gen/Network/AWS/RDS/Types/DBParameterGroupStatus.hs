{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBParameterGroupStatus
  ( DBParameterGroupStatus (..)
  -- * Smart constructor
  , mkDBParameterGroupStatus
  -- * Lenses
  , dbpgsDBParameterGroupName
  , dbpgsParameterApplyStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the DB parameter group.
--
-- This data type is used as a response element in the following actions:
--
--     * @CreateDBInstance@ 
--
--
--     * @CreateDBInstanceReadReplica@ 
--
--
--     * @DeleteDBInstance@ 
--
--
--     * @ModifyDBInstance@ 
--
--
--     * @RebootDBInstance@ 
--
--
--     * @RestoreDBInstanceFromDBSnapshot@ 
--
--
--
-- /See:/ 'mkDBParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
  { dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group.
  , parameterApplyStatus :: Core.Maybe Core.Text
    -- ^ The status of parameter updates.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBParameterGroupStatus' value with any optional fields omitted.
mkDBParameterGroupStatus
    :: DBParameterGroupStatus
mkDBParameterGroupStatus
  = DBParameterGroupStatus'{dBParameterGroupName = Core.Nothing,
                            parameterApplyStatus = Core.Nothing}

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgsDBParameterGroupName :: Lens.Lens' DBParameterGroupStatus (Core.Maybe Core.Text)
dbpgsDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE dbpgsDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgsParameterApplyStatus :: Lens.Lens' DBParameterGroupStatus (Core.Maybe Core.Text)
dbpgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# INLINEABLE dbpgsParameterApplyStatus #-}
{-# DEPRECATED parameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead"  #-}

instance Core.FromXML DBParameterGroupStatus where
        parseXML x
          = DBParameterGroupStatus' Core.<$>
              (x Core..@? "DBParameterGroupName") Core.<*>
                x Core..@? "ParameterApplyStatus"
