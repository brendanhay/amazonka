{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshotAttributesResult
  ( DBSnapshotAttributesResult (..),

    -- * Smart constructor
    mkDBSnapshotAttributesResult,

    -- * Lenses
    dsarDBSnapshotIdentifier,
    dsarDBSnapshotAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.DBSnapshotAttribute

-- | Contains the results of a successful call to the @DescribeDBSnapshotAttributes@ API action.
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { dbSnapshotIdentifier ::
      Lude.Maybe Lude.Text,
    dbSnapshotAttributes ::
      Lude.Maybe [DBSnapshotAttribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- * 'dbSnapshotAttributes' - The list of attributes and values for the manual DB snapshot.
-- * 'dbSnapshotIdentifier' - The identifier of the manual DB snapshot that the attributes apply to.
mkDBSnapshotAttributesResult ::
  DBSnapshotAttributesResult
mkDBSnapshotAttributesResult =
  DBSnapshotAttributesResult'
    { dbSnapshotIdentifier = Lude.Nothing,
      dbSnapshotAttributes = Lude.Nothing
    }

-- | The identifier of the manual DB snapshot that the attributes apply to.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarDBSnapshotIdentifier :: Lens.Lens' DBSnapshotAttributesResult (Lude.Maybe Lude.Text)
dsarDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: DBSnapshotAttributesResult -> Lude.Maybe Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: DBSnapshotAttributesResult)
{-# DEPRECATED dsarDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

-- | The list of attributes and values for the manual DB snapshot.
--
-- /Note:/ Consider using 'dbSnapshotAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarDBSnapshotAttributes :: Lens.Lens' DBSnapshotAttributesResult (Lude.Maybe [DBSnapshotAttribute])
dsarDBSnapshotAttributes = Lens.lens (dbSnapshotAttributes :: DBSnapshotAttributesResult -> Lude.Maybe [DBSnapshotAttribute]) (\s a -> s {dbSnapshotAttributes = a} :: DBSnapshotAttributesResult)
{-# DEPRECATED dsarDBSnapshotAttributes "Use generic-lens or generic-optics with 'dbSnapshotAttributes' instead." #-}

instance Lude.FromXML DBSnapshotAttributesResult where
  parseXML x =
    DBSnapshotAttributesResult'
      Lude.<$> (x Lude..@? "DBSnapshotIdentifier")
      Lude.<*> ( x Lude..@? "DBSnapshotAttributes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBSnapshotAttribute")
               )
