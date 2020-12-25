{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroupStatus
  ( DBParameterGroupStatus (..),

    -- * Smart constructor
    mkDBParameterGroupStatus,

    -- * Lenses
    dbpgsDBParameterGroupName,
    dbpgsParameterApplyStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBParameterGroupName as Types
import qualified Network.AWS.RDS.Types.ParameterApplyStatus as Types

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
  { -- | The name of the DB parameter group.
    dBParameterGroupName :: Core.Maybe Types.DBParameterGroupName,
    -- | The status of parameter updates.
    parameterApplyStatus :: Core.Maybe Types.ParameterApplyStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBParameterGroupStatus' value with any optional fields omitted.
mkDBParameterGroupStatus ::
  DBParameterGroupStatus
mkDBParameterGroupStatus =
  DBParameterGroupStatus'
    { dBParameterGroupName = Core.Nothing,
      parameterApplyStatus = Core.Nothing
    }

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgsDBParameterGroupName :: Lens.Lens' DBParameterGroupStatus (Core.Maybe Types.DBParameterGroupName)
dbpgsDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# DEPRECATED dbpgsDBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpgsParameterApplyStatus :: Lens.Lens' DBParameterGroupStatus (Core.Maybe Types.ParameterApplyStatus)
dbpgsParameterApplyStatus = Lens.field @"parameterApplyStatus"
{-# DEPRECATED dbpgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

instance Core.FromXML DBParameterGroupStatus where
  parseXML x =
    DBParameterGroupStatus'
      Core.<$> (x Core..@? "DBParameterGroupName")
      Core.<*> (x Core..@? "ParameterApplyStatus")
