{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB cluster snapshot attribute names and values for a manual DB cluster snapshot.
--
-- When sharing snapshots with other AWS accounts, @DescribeDBClusterSnapshotAttributes@ returns the @restore@ attribute and a list of IDs for the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If @all@ is included in the list of values for the @restore@ attribute, then the manual DB cluster snapshot is public and can be copied or restored by all AWS accounts.
-- To add or remove access for an AWS account to copy or restore a manual DB cluster snapshot, or to make the manual DB cluster snapshot public or private, use the @ModifyDBClusterSnapshotAttribute@ API action.
module Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
    (
    -- * Creating a request
      DescribeDBClusterSnapshotAttributes (..)
    , mkDescribeDBClusterSnapshotAttributes
    -- ** Request lenses
    , ddbcsaDBClusterSnapshotIdentifier

    -- * Destructuring the response
    , DescribeDBClusterSnapshotAttributesResponse (..)
    , mkDescribeDBClusterSnapshotAttributesResponse
    -- ** Response lenses
    , ddbcsarrsDBClusterSnapshotAttributesResult
    , ddbcsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDescribeDBClusterSnapshotAttributes' smart constructor.
newtype DescribeDBClusterSnapshotAttributes = DescribeDBClusterSnapshotAttributes'
  { dBClusterSnapshotIdentifier :: Core.Text
    -- ^ The identifier for the DB cluster snapshot to describe the attributes for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterSnapshotAttributes' value with any optional fields omitted.
mkDescribeDBClusterSnapshotAttributes
    :: Core.Text -- ^ 'dBClusterSnapshotIdentifier'
    -> DescribeDBClusterSnapshotAttributes
mkDescribeDBClusterSnapshotAttributes dBClusterSnapshotIdentifier
  = DescribeDBClusterSnapshotAttributes'{dBClusterSnapshotIdentifier}

-- | The identifier for the DB cluster snapshot to describe the attributes for.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsaDBClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshotAttributes Core.Text
ddbcsaDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# INLINEABLE ddbcsaDBClusterSnapshotIdentifier #-}
{-# DEPRECATED dBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead"  #-}

instance Core.ToQuery DescribeDBClusterSnapshotAttributes where
        toQuery DescribeDBClusterSnapshotAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeDBClusterSnapshotAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterSnapshotIdentifier"
                dBClusterSnapshotIdentifier

instance Core.ToHeaders DescribeDBClusterSnapshotAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDBClusterSnapshotAttributes where
        type Rs DescribeDBClusterSnapshotAttributes =
             DescribeDBClusterSnapshotAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "DescribeDBClusterSnapshotAttributesResult"
              (\ s h x ->
                 DescribeDBClusterSnapshotAttributesResponse' Core.<$>
                   (x Core..@? "DBClusterSnapshotAttributesResult") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDBClusterSnapshotAttributesResponse' smart constructor.
data DescribeDBClusterSnapshotAttributesResponse = DescribeDBClusterSnapshotAttributesResponse'
  { dBClusterSnapshotAttributesResult :: Core.Maybe Types.DBClusterSnapshotAttributesResult
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBClusterSnapshotAttributesResponse' value with any optional fields omitted.
mkDescribeDBClusterSnapshotAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDBClusterSnapshotAttributesResponse
mkDescribeDBClusterSnapshotAttributesResponse responseStatus
  = DescribeDBClusterSnapshotAttributesResponse'{dBClusterSnapshotAttributesResult
                                                   = Core.Nothing,
                                                 responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsarrsDBClusterSnapshotAttributesResult :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse (Core.Maybe Types.DBClusterSnapshotAttributesResult)
ddbcsarrsDBClusterSnapshotAttributesResult = Lens.field @"dBClusterSnapshotAttributesResult"
{-# INLINEABLE ddbcsarrsDBClusterSnapshotAttributesResult #-}
{-# DEPRECATED dBClusterSnapshotAttributesResult "Use generic-lens or generic-optics with 'dBClusterSnapshotAttributesResult' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbcsarrsResponseStatus :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse Core.Int
ddbcsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddbcsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
