{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterOptionGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterOptionGroupStatus
  ( DBClusterOptionGroupStatus (..),

    -- * Smart constructor
    mkDBClusterOptionGroupStatus,

    -- * Lenses
    dcogsStatus,
    dcogsDBClusterOptionGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains status information for a DB cluster option group.
--
-- /See:/ 'mkDBClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
  { -- | Specifies the status of the DB cluster option group.
    status :: Lude.Maybe Lude.Text,
    -- | Specifies the name of the DB cluster option group.
    dbClusterOptionGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterOptionGroupStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the DB cluster option group.
-- * 'dbClusterOptionGroupName' - Specifies the name of the DB cluster option group.
mkDBClusterOptionGroupStatus ::
  DBClusterOptionGroupStatus
mkDBClusterOptionGroupStatus =
  DBClusterOptionGroupStatus'
    { status = Lude.Nothing,
      dbClusterOptionGroupName = Lude.Nothing
    }

-- | Specifies the status of the DB cluster option group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcogsStatus :: Lens.Lens' DBClusterOptionGroupStatus (Lude.Maybe Lude.Text)
dcogsStatus = Lens.lens (status :: DBClusterOptionGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBClusterOptionGroupStatus)
{-# DEPRECATED dcogsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the name of the DB cluster option group.
--
-- /Note:/ Consider using 'dbClusterOptionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcogsDBClusterOptionGroupName :: Lens.Lens' DBClusterOptionGroupStatus (Lude.Maybe Lude.Text)
dcogsDBClusterOptionGroupName = Lens.lens (dbClusterOptionGroupName :: DBClusterOptionGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterOptionGroupName = a} :: DBClusterOptionGroupStatus)
{-# DEPRECATED dcogsDBClusterOptionGroupName "Use generic-lens or generic-optics with 'dbClusterOptionGroupName' instead." #-}

instance Lude.FromXML DBClusterOptionGroupStatus where
  parseXML x =
    DBClusterOptionGroupStatus'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBClusterOptionGroupName")
