{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBInstanceStatusInfo
  ( DBInstanceStatusInfo (..)
  -- * Smart constructor
  , mkDBInstanceStatusInfo
  -- * Lenses
  , dbisiMessage
  , dbisiNormal
  , dbisiStatus
  , dbisiStatusType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides a list of status information for a DB instance.
--
-- /See:/ 'mkDBInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { message :: Core.Maybe Core.Text
    -- ^ Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
  , normal :: Core.Maybe Core.Bool
    -- ^ Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
  , status :: Core.Maybe Core.Text
    -- ^ Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
  , statusType :: Core.Maybe Core.Text
    -- ^ This value is currently "read replication."
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBInstanceStatusInfo' value with any optional fields omitted.
mkDBInstanceStatusInfo
    :: DBInstanceStatusInfo
mkDBInstanceStatusInfo
  = DBInstanceStatusInfo'{message = Core.Nothing,
                          normal = Core.Nothing, status = Core.Nothing,
                          statusType = Core.Nothing}

-- | Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbisiMessage :: Lens.Lens' DBInstanceStatusInfo (Core.Maybe Core.Text)
dbisiMessage = Lens.field @"message"
{-# INLINEABLE dbisiMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
--
-- /Note:/ Consider using 'normal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbisiNormal :: Lens.Lens' DBInstanceStatusInfo (Core.Maybe Core.Bool)
dbisiNormal = Lens.field @"normal"
{-# INLINEABLE dbisiNormal #-}
{-# DEPRECATED normal "Use generic-lens or generic-optics with 'normal' instead"  #-}

-- | Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbisiStatus :: Lens.Lens' DBInstanceStatusInfo (Core.Maybe Core.Text)
dbisiStatus = Lens.field @"status"
{-# INLINEABLE dbisiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | This value is currently "read replication."
--
-- /Note:/ Consider using 'statusType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbisiStatusType :: Lens.Lens' DBInstanceStatusInfo (Core.Maybe Core.Text)
dbisiStatusType = Lens.field @"statusType"
{-# INLINEABLE dbisiStatusType #-}
{-# DEPRECATED statusType "Use generic-lens or generic-optics with 'statusType' instead"  #-}

instance Core.FromXML DBInstanceStatusInfo where
        parseXML x
          = DBInstanceStatusInfo' Core.<$>
              (x Core..@? "Message") Core.<*> x Core..@? "Normal" Core.<*>
                x Core..@? "Status"
                Core.<*> x Core..@? "StatusType"
