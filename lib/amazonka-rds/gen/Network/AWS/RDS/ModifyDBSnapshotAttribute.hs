{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values from, a manual DB snapshot.
--
-- To share a manual DB snapshot with other AWS accounts, specify @restore@ as the @AttributeName@ and use the @ValuesToAdd@ parameter to add a list of IDs of the AWS accounts that are authorized to restore the manual DB snapshot. Uses the value @all@ to make the manual DB snapshot public, which means it can be copied or restored by all AWS accounts.
-- If the manual DB snapshot is encrypted, it can be shared, but only by specifying a list of authorized AWS account IDs for the @ValuesToAdd@ parameter. You can't use @all@ as a value for that parameter in this case.
-- To view which AWS accounts have access to copy or restore a manual DB snapshot, or whether a manual DB snapshot public or private, use the 'DescribeDBSnapshotAttributes' API action. The accounts are returned as values for the @restore@ attribute.
module Network.AWS.RDS.ModifyDBSnapshotAttribute
  ( -- * Creating a request
    ModifyDBSnapshotAttribute (..),
    mkModifyDBSnapshotAttribute,

    -- ** Request lenses
    mdbsaDBSnapshotIdentifier,
    mdbsaAttributeName,
    mdbsaValuesToAdd,
    mdbsaValuesToRemove,

    -- * Destructuring the response
    ModifyDBSnapshotAttributeResponse (..),
    mkModifyDBSnapshotAttributeResponse,

    -- ** Response lenses
    mdbsarrsDBSnapshotAttributesResult,
    mdbsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkModifyDBSnapshotAttribute' smart constructor.
data ModifyDBSnapshotAttribute = ModifyDBSnapshotAttribute'
  { -- | The identifier for the DB snapshot to modify the attributes for.
    dBSnapshotIdentifier :: Types.String,
    -- | The name of the DB snapshot attribute to modify.
    --
    -- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
    attributeName :: Types.String,
    -- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
    --
    -- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
    valuesToAdd :: Core.Maybe [Types.String],
    -- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
    --
    -- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
    valuesToRemove :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshotAttribute' value with any optional fields omitted.
mkModifyDBSnapshotAttribute ::
  -- | 'dBSnapshotIdentifier'
  Types.String ->
  -- | 'attributeName'
  Types.String ->
  ModifyDBSnapshotAttribute
mkModifyDBSnapshotAttribute dBSnapshotIdentifier attributeName =
  ModifyDBSnapshotAttribute'
    { dBSnapshotIdentifier,
      attributeName,
      valuesToAdd = Core.Nothing,
      valuesToRemove = Core.Nothing
    }

-- | The identifier for the DB snapshot to modify the attributes for.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshotAttribute Types.String
mdbsaDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# DEPRECATED mdbsaDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead." #-}

-- | The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaAttributeName :: Lens.Lens' ModifyDBSnapshotAttribute Types.String
mdbsaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED mdbsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
--
-- /Note:/ Consider using 'valuesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaValuesToAdd :: Lens.Lens' ModifyDBSnapshotAttribute (Core.Maybe [Types.String])
mdbsaValuesToAdd = Lens.field @"valuesToAdd"
{-# DEPRECATED mdbsaValuesToAdd "Use generic-lens or generic-optics with 'valuesToAdd' instead." #-}

-- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
--
-- /Note:/ Consider using 'valuesToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaValuesToRemove :: Lens.Lens' ModifyDBSnapshotAttribute (Core.Maybe [Types.String])
mdbsaValuesToRemove = Lens.field @"valuesToRemove"
{-# DEPRECATED mdbsaValuesToRemove "Use generic-lens or generic-optics with 'valuesToRemove' instead." #-}

instance Core.AWSRequest ModifyDBSnapshotAttribute where
  type
    Rs ModifyDBSnapshotAttribute =
      ModifyDBSnapshotAttributeResponse
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
            ( Core.pure ("Action", "ModifyDBSnapshotAttribute")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DBSnapshotIdentifier" dBSnapshotIdentifier)
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
      "ModifyDBSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBSnapshotAttributeResponse'
            Core.<$> (x Core..@? "DBSnapshotAttributesResult")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyDBSnapshotAttributeResponse' smart constructor.
data ModifyDBSnapshotAttributeResponse = ModifyDBSnapshotAttributeResponse'
  { dBSnapshotAttributesResult :: Core.Maybe Types.DBSnapshotAttributesResult,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshotAttributeResponse' value with any optional fields omitted.
mkModifyDBSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyDBSnapshotAttributeResponse
mkModifyDBSnapshotAttributeResponse responseStatus =
  ModifyDBSnapshotAttributeResponse'
    { dBSnapshotAttributesResult =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsarrsDBSnapshotAttributesResult :: Lens.Lens' ModifyDBSnapshotAttributeResponse (Core.Maybe Types.DBSnapshotAttributesResult)
mdbsarrsDBSnapshotAttributesResult = Lens.field @"dBSnapshotAttributesResult"
{-# DEPRECATED mdbsarrsDBSnapshotAttributesResult "Use generic-lens or generic-optics with 'dBSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsarrsResponseStatus :: Lens.Lens' ModifyDBSnapshotAttributeResponse Core.Int
mdbsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mdbsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
