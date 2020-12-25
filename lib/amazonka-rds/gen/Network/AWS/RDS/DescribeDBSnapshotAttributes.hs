{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSnapshotAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB snapshot attribute names and values for a manual DB snapshot.
--
-- When sharing snapshots with other AWS accounts, @DescribeDBSnapshotAttributes@ returns the @restore@ attribute and a list of IDs for the AWS accounts that are authorized to copy or restore the manual DB snapshot. If @all@ is included in the list of values for the @restore@ attribute, then the manual DB snapshot is public and can be copied or restored by all AWS accounts.
-- To add or remove access for an AWS account to copy or restore a manual DB snapshot, or to make the manual DB snapshot public or private, use the @ModifyDBSnapshotAttribute@ API action.
module Network.AWS.RDS.DescribeDBSnapshotAttributes
  ( -- * Creating a request
    DescribeDBSnapshotAttributes (..),
    mkDescribeDBSnapshotAttributes,

    -- ** Request lenses
    ddbsaDBSnapshotIdentifier,

    -- * Destructuring the response
    DescribeDBSnapshotAttributesResponse (..),
    mkDescribeDBSnapshotAttributesResponse,

    -- ** Response lenses
    ddbsarrsDBSnapshotAttributesResult,
    ddbsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeDBSnapshotAttributes' smart constructor.
newtype DescribeDBSnapshotAttributes = DescribeDBSnapshotAttributes'
  { -- | The identifier for the DB snapshot to describe the attributes for.
    dBSnapshotIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSnapshotAttributes' value with any optional fields omitted.
mkDescribeDBSnapshotAttributes ::
  -- | 'dBSnapshotIdentifier'
  Types.String ->
  DescribeDBSnapshotAttributes
mkDescribeDBSnapshotAttributes dBSnapshotIdentifier =
  DescribeDBSnapshotAttributes' {dBSnapshotIdentifier}

-- | The identifier for the DB snapshot to describe the attributes for.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsaDBSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshotAttributes Types.String
ddbsaDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED ddbsaDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

instance Core.AWSRequest DescribeDBSnapshotAttributes where
  type
    Rs DescribeDBSnapshotAttributes =
      DescribeDBSnapshotAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeDBSnapshotAttributes")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSnapshotIdentifier" dBSnapshotIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeDBSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBSnapshotAttributesResponse'
            Core.<$> (x Core..@? "DBSnapshotAttributesResult")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDBSnapshotAttributesResponse' smart constructor.
data DescribeDBSnapshotAttributesResponse = DescribeDBSnapshotAttributesResponse'
  { dBSnapshotAttributesResult :: Core.Maybe Types.DBSnapshotAttributesResult,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDBSnapshotAttributesResponse' value with any optional fields omitted.
mkDescribeDBSnapshotAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDBSnapshotAttributesResponse
mkDescribeDBSnapshotAttributesResponse responseStatus =
  DescribeDBSnapshotAttributesResponse'
    { dBSnapshotAttributesResult =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsarrsDBSnapshotAttributesResult :: Lens.Lens' DescribeDBSnapshotAttributesResponse (Core.Maybe Types.DBSnapshotAttributesResult)
ddbsarrsDBSnapshotAttributesResult = Lens.field @"dBSnapshotAttributesResult"
{-# DEPRECATED ddbsarrsDBSnapshotAttributesResult "Use generic-lens or generic-optics with 'dBSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbsarrsResponseStatus :: Lens.Lens' DescribeDBSnapshotAttributesResponse Core.Int
ddbsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddbsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
