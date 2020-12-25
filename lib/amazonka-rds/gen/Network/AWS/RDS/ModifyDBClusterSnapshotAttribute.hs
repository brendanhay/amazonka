{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values from, a manual DB cluster snapshot.
--
-- To share a manual DB cluster snapshot with other AWS accounts, specify @restore@ as the @AttributeName@ and use the @ValuesToAdd@ parameter to add a list of IDs of the AWS accounts that are authorized to restore the manual DB cluster snapshot. Use the value @all@ to make the manual DB cluster snapshot public, which means that it can be copied or restored by all AWS accounts.
-- If a manual DB cluster snapshot is encrypted, it can be shared, but only by specifying a list of authorized AWS account IDs for the @ValuesToAdd@ parameter. You can't use @all@ as a value for that parameter in this case.
-- To view which AWS accounts have access to copy or restore a manual DB cluster snapshot, or whether a manual DB cluster snapshot is public or private, use the 'DescribeDBClusterSnapshotAttributes' API action. The accounts are returned as values for the @restore@ attribute.
module Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
  ( -- * Creating a request
    ModifyDBClusterSnapshotAttribute (..),
    mkModifyDBClusterSnapshotAttribute,

    -- ** Request lenses
    mdbcsaDBClusterSnapshotIdentifier,
    mdbcsaAttributeName,
    mdbcsaValuesToAdd,
    mdbcsaValuesToRemove,

    -- * Destructuring the response
    ModifyDBClusterSnapshotAttributeResponse (..),
    mkModifyDBClusterSnapshotAttributeResponse,

    -- ** Response lenses
    mdbcsarrsDBClusterSnapshotAttributesResult,
    mdbcsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyDBClusterSnapshotAttribute' smart constructor.
data ModifyDBClusterSnapshotAttribute = ModifyDBClusterSnapshotAttribute'
  { -- | The identifier for the DB cluster snapshot to modify the attributes for.
    dBClusterSnapshotIdentifier :: Types.DBClusterSnapshotIdentifier,
    -- | The name of the DB cluster snapshot attribute to modify.
    --
    -- To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
    attributeName :: Types.AttributeName,
    -- | A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ .
    --
    -- To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
    valuesToAdd :: Core.Maybe [Types.String],
    -- | A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ .
    --
    -- To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
    valuesToRemove :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBClusterSnapshotAttribute' value with any optional fields omitted.
mkModifyDBClusterSnapshotAttribute ::
  -- | 'dBClusterSnapshotIdentifier'
  Types.DBClusterSnapshotIdentifier ->
  -- | 'attributeName'
  Types.AttributeName ->
  ModifyDBClusterSnapshotAttribute
mkModifyDBClusterSnapshotAttribute
  dBClusterSnapshotIdentifier
  attributeName =
    ModifyDBClusterSnapshotAttribute'
      { dBClusterSnapshotIdentifier,
        attributeName,
        valuesToAdd = Core.Nothing,
        valuesToRemove = Core.Nothing
      }

-- | The identifier for the DB cluster snapshot to modify the attributes for.
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsaDBClusterSnapshotIdentifier :: Lens.Lens' ModifyDBClusterSnapshotAttribute Types.DBClusterSnapshotIdentifier
mdbcsaDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# DEPRECATED mdbcsaDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead." #-}

-- | The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsaAttributeName :: Lens.Lens' ModifyDBClusterSnapshotAttribute Types.AttributeName
mdbcsaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED mdbcsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
--
-- /Note:/ Consider using 'valuesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsaValuesToAdd :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Core.Maybe [Types.String])
mdbcsaValuesToAdd = Lens.field @"valuesToAdd"
{-# DEPRECATED mdbcsaValuesToAdd "Use generic-lens or generic-optics with 'valuesToAdd' instead." #-}

-- | A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
--
-- /Note:/ Consider using 'valuesToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsaValuesToRemove :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Core.Maybe [Types.String])
mdbcsaValuesToRemove = Lens.field @"valuesToRemove"
{-# DEPRECATED mdbcsaValuesToRemove "Use generic-lens or generic-optics with 'valuesToRemove' instead." #-}

instance Core.AWSRequest ModifyDBClusterSnapshotAttribute where
  type
    Rs ModifyDBClusterSnapshotAttribute =
      ModifyDBClusterSnapshotAttributeResponse
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
            ( Core.pure ("Action", "ModifyDBClusterSnapshotAttribute")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterSnapshotIdentifier"
                            dBClusterSnapshotIdentifier
                        )
                Core.<> (Core.toQueryValue "AttributeName" attributeName)
                Core.<> ( Core.toQueryValue
                            "ValuesToAdd"
                            (Core.toQueryList "AttributeValue" Core.<$> valuesToAdd)
                        )
                Core.<> ( Core.toQueryValue
                            "ValuesToRemove"
                            (Core.toQueryList "AttributeValue" Core.<$> valuesToRemove)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBClusterSnapshotAttributeResponse'
            Core.<$> (x Core..@? "DBClusterSnapshotAttributesResult")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBClusterSnapshotAttributeResponse' smart constructor.
data ModifyDBClusterSnapshotAttributeResponse = ModifyDBClusterSnapshotAttributeResponse'
  { dBClusterSnapshotAttributesResult :: Core.Maybe Types.DBClusterSnapshotAttributesResult,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBClusterSnapshotAttributeResponse' value with any optional fields omitted.
mkModifyDBClusterSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBClusterSnapshotAttributeResponse
mkModifyDBClusterSnapshotAttributeResponse responseStatus =
  ModifyDBClusterSnapshotAttributeResponse'
    { dBClusterSnapshotAttributesResult =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsarrsDBClusterSnapshotAttributesResult :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse (Core.Maybe Types.DBClusterSnapshotAttributesResult)
mdbcsarrsDBClusterSnapshotAttributesResult = Lens.field @"dBClusterSnapshotAttributesResult"
{-# DEPRECATED mdbcsarrsDBClusterSnapshotAttributesResult "Use generic-lens or generic-optics with 'dBClusterSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbcsarrsResponseStatus :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse Core.Int
mdbcsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbcsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
