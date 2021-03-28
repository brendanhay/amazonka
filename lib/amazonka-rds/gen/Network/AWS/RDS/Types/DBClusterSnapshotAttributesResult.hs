{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
  ( DBClusterSnapshotAttributesResult (..)
  -- * Smart constructor
  , mkDBClusterSnapshotAttributesResult
  -- * Lenses
  , dbcsarDBClusterSnapshotAttributes
  , dbcsarDBClusterSnapshotIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBClusterSnapshotAttribute as Types

-- | Contains the results of a successful call to the @DescribeDBClusterSnapshotAttributes@ API action.
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { dBClusterSnapshotAttributes :: Core.Maybe [Types.DBClusterSnapshotAttribute]
    -- ^ The list of attributes and values for the manual DB cluster snapshot.
  , dBClusterSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the manual DB cluster snapshot that the attributes apply to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterSnapshotAttributesResult' value with any optional fields omitted.
mkDBClusterSnapshotAttributesResult
    :: DBClusterSnapshotAttributesResult
mkDBClusterSnapshotAttributesResult
  = DBClusterSnapshotAttributesResult'{dBClusterSnapshotAttributes =
                                         Core.Nothing,
                                       dBClusterSnapshotIdentifier = Core.Nothing}

-- | The list of attributes and values for the manual DB cluster snapshot.
--
-- /Note:/ Consider using 'dBClusterSnapshotAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsarDBClusterSnapshotAttributes :: Lens.Lens' DBClusterSnapshotAttributesResult (Core.Maybe [Types.DBClusterSnapshotAttribute])
dbcsarDBClusterSnapshotAttributes = Lens.field @"dBClusterSnapshotAttributes"
{-# INLINEABLE dbcsarDBClusterSnapshotAttributes #-}
{-# DEPRECATED dBClusterSnapshotAttributes "Use generic-lens or generic-optics with 'dBClusterSnapshotAttributes' instead"  #-}

-- | The identifier of the manual DB cluster snapshot that the attributes apply to.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsarDBClusterSnapshotIdentifier :: Lens.Lens' DBClusterSnapshotAttributesResult (Core.Maybe Core.Text)
dbcsarDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE dbcsarDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

instance Core.FromXML DBClusterSnapshotAttributesResult where
        parseXML x
          = DBClusterSnapshotAttributesResult' Core.<$>
              (x Core..@? "DBClusterSnapshotAttributes" Core..<@>
                 Core.parseXMLList "DBClusterSnapshotAttribute")
                Core.<*> x Core..@? "DBClusterSnapshotIdentifier"
