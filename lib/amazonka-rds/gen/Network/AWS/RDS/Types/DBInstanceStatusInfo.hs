{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBInstanceStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBInstanceStatusInfo
  ( DBInstanceStatusInfo (..),

    -- * Smart constructor
    mkDBInstanceStatusInfo,

    -- * Lenses
    disiStatus,
    disiNormal,
    disiStatusType,
    disiMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a list of status information for a DB instance.
--
-- /See:/ 'mkDBInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { status ::
      Lude.Maybe Lude.Text,
    normal :: Lude.Maybe Lude.Bool,
    statusType :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBInstanceStatusInfo' with the minimum fields required to make a request.
--
-- * 'message' - Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
-- * 'normal' - Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
-- * 'status' - Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
-- * 'statusType' - This value is currently "read replication."
mkDBInstanceStatusInfo ::
  DBInstanceStatusInfo
mkDBInstanceStatusInfo =
  DBInstanceStatusInfo'
    { status = Lude.Nothing,
      normal = Lude.Nothing,
      statusType = Lude.Nothing,
      message = Lude.Nothing
    }

-- | Status of the DB instance. For a StatusType of read replica, the values can be replicating, replication stop point set, replication stop point reached, error, stopped, or terminated.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disiStatus :: Lens.Lens' DBInstanceStatusInfo (Lude.Maybe Lude.Text)
disiStatus = Lens.lens (status :: DBInstanceStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBInstanceStatusInfo)
{-# DEPRECATED disiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
--
-- /Note:/ Consider using 'normal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disiNormal :: Lens.Lens' DBInstanceStatusInfo (Lude.Maybe Lude.Bool)
disiNormal = Lens.lens (normal :: DBInstanceStatusInfo -> Lude.Maybe Lude.Bool) (\s a -> s {normal = a} :: DBInstanceStatusInfo)
{-# DEPRECATED disiNormal "Use generic-lens or generic-optics with 'normal' instead." #-}

-- | This value is currently "read replication."
--
-- /Note:/ Consider using 'statusType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disiStatusType :: Lens.Lens' DBInstanceStatusInfo (Lude.Maybe Lude.Text)
disiStatusType = Lens.lens (statusType :: DBInstanceStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {statusType = a} :: DBInstanceStatusInfo)
{-# DEPRECATED disiStatusType "Use generic-lens or generic-optics with 'statusType' instead." #-}

-- | Details of the error if there is an error for the instance. If the instance isn't in an error state, this value is blank.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disiMessage :: Lens.Lens' DBInstanceStatusInfo (Lude.Maybe Lude.Text)
disiMessage = Lens.lens (message :: DBInstanceStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DBInstanceStatusInfo)
{-# DEPRECATED disiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML DBInstanceStatusInfo where
  parseXML x =
    DBInstanceStatusInfo'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "Normal")
      Lude.<*> (x Lude..@? "StatusType")
      Lude.<*> (x Lude..@? "Message")
