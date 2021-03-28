{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttributesResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBSnapshotAttributesResult
  ( DBSnapshotAttributesResult (..)
  -- * Smart constructor
  , mkDBSnapshotAttributesResult
  -- * Lenses
  , dbsarDBSnapshotAttributes
  , dbsarDBSnapshotIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBSnapshotAttribute as Types

-- | Contains the results of a successful call to the @DescribeDBSnapshotAttributes@ API action.
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { dBSnapshotAttributes :: Core.Maybe [Types.DBSnapshotAttribute]
    -- ^ The list of attributes and values for the manual DB snapshot.
  , dBSnapshotIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the manual DB snapshot that the attributes apply to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSnapshotAttributesResult' value with any optional fields omitted.
mkDBSnapshotAttributesResult
    :: DBSnapshotAttributesResult
mkDBSnapshotAttributesResult
  = DBSnapshotAttributesResult'{dBSnapshotAttributes = Core.Nothing,
                                dBSnapshotIdentifier = Core.Nothing}

-- | The list of attributes and values for the manual DB snapshot.
--
-- /Note:/ Consider using 'dBSnapshotAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsarDBSnapshotAttributes :: Lens.Lens' DBSnapshotAttributesResult (Core.Maybe [Types.DBSnapshotAttribute])
dbsarDBSnapshotAttributes = Lens.field @"dBSnapshotAttributes"
{-# INLINEABLE dbsarDBSnapshotAttributes #-}
{-# DEPRECATED dBSnapshotAttributes "Use generic-lens or generic-optics with 'dBSnapshotAttributes' instead"  #-}

-- | The identifier of the manual DB snapshot that the attributes apply to.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsarDBSnapshotIdentifier :: Lens.Lens' DBSnapshotAttributesResult (Core.Maybe Core.Text)
dbsarDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE dbsarDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

instance Core.FromXML DBSnapshotAttributesResult where
        parseXML x
          = DBSnapshotAttributesResult' Core.<$>
              (x Core..@? "DBSnapshotAttributes" Core..<@>
                 Core.parseXMLList "DBSnapshotAttribute")
                Core.<*> x Core..@? "DBSnapshotIdentifier"
