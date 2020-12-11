-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
  ( DBClusterSnapshotAttributesResult (..),

    -- * Smart constructor
    mkDBClusterSnapshotAttributesResult,

    -- * Lenses
    dcsarDBClusterSnapshotIdentifier,
    dcsarDBClusterSnapshotAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute

-- | Contains the results of a successful call to the @DescribeDBClusterSnapshotAttributes@ API action.
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { dbClusterSnapshotIdentifier ::
      Lude.Maybe Lude.Text,
    dbClusterSnapshotAttributes ::
      Lude.Maybe
        [DBClusterSnapshotAttribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshotAttributes' - The list of attributes and values for the manual DB cluster snapshot.
-- * 'dbClusterSnapshotIdentifier' - The identifier of the manual DB cluster snapshot that the attributes apply to.
mkDBClusterSnapshotAttributesResult ::
  DBClusterSnapshotAttributesResult
mkDBClusterSnapshotAttributesResult =
  DBClusterSnapshotAttributesResult'
    { dbClusterSnapshotIdentifier =
        Lude.Nothing,
      dbClusterSnapshotAttributes = Lude.Nothing
    }

-- | The identifier of the manual DB cluster snapshot that the attributes apply to.
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsarDBClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshotAttributesResult (Lude.Maybe Lude.Text)
dcsarDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: DBClusterSnapshotAttributesResult -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: DBClusterSnapshotAttributesResult)
{-# DEPRECATED dcsarDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

-- | The list of attributes and values for the manual DB cluster snapshot.
--
-- /Note:/ Consider using 'dbClusterSnapshotAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsarDBClusterSnapshotAttributes :: Lens.Lens' DBClusterSnapshotAttributesResult (Lude.Maybe [DBClusterSnapshotAttribute])
dcsarDBClusterSnapshotAttributes = Lens.lens (dbClusterSnapshotAttributes :: DBClusterSnapshotAttributesResult -> Lude.Maybe [DBClusterSnapshotAttribute]) (\s a -> s {dbClusterSnapshotAttributes = a} :: DBClusterSnapshotAttributesResult)
{-# DEPRECATED dcsarDBClusterSnapshotAttributes "Use generic-lens or generic-optics with 'dbClusterSnapshotAttributes' instead." #-}

instance Lude.FromXML DBClusterSnapshotAttributesResult where
  parseXML x =
    DBClusterSnapshotAttributesResult'
      Lude.<$> (x Lude..@? "DBClusterSnapshotIdentifier")
      Lude.<*> ( x Lude..@? "DBClusterSnapshotAttributes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBClusterSnapshotAttribute")
               )
