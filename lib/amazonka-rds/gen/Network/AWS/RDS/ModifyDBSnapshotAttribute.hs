{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyDBSnapshotAttribute (..)
    , mkModifyDBSnapshotAttribute
    -- ** Request lenses
    , mdbsaDBSnapshotIdentifier
    , mdbsaAttributeName
    , mdbsaValuesToAdd
    , mdbsaValuesToRemove

    -- * Destructuring the response
    , ModifyDBSnapshotAttributeResponse (..)
    , mkModifyDBSnapshotAttributeResponse
    -- ** Response lenses
    , mdbsarrsDBSnapshotAttributesResult
    , mdbsarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkModifyDBSnapshotAttribute' smart constructor.
data ModifyDBSnapshotAttribute = ModifyDBSnapshotAttribute'
  { dBSnapshotIdentifier :: Core.Text
    -- ^ The identifier for the DB snapshot to modify the attributes for.
  , attributeName :: Core.Text
    -- ^ The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
  , valuesToAdd :: Core.Maybe [Core.Text]
    -- ^ A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
  , valuesToRemove :: Core.Maybe [Core.Text]
    -- ^ A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshotAttribute' value with any optional fields omitted.
mkModifyDBSnapshotAttribute
    :: Core.Text -- ^ 'dBSnapshotIdentifier'
    -> Core.Text -- ^ 'attributeName'
    -> ModifyDBSnapshotAttribute
mkModifyDBSnapshotAttribute dBSnapshotIdentifier attributeName
  = ModifyDBSnapshotAttribute'{dBSnapshotIdentifier, attributeName,
                               valuesToAdd = Core.Nothing, valuesToRemove = Core.Nothing}

-- | The identifier for the DB snapshot to modify the attributes for.
--
-- /Note:/ Consider using 'dBSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshotAttribute Core.Text
mdbsaDBSnapshotIdentifier = Lens.field @"dBSnapshotIdentifier"
{-# INLINEABLE mdbsaDBSnapshotIdentifier #-}
{-# DEPRECATED dBSnapshotIdentifier "Use generic-lens or generic-optics with 'dBSnapshotIdentifier' instead"  #-}

-- | The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaAttributeName :: Lens.Lens' ModifyDBSnapshotAttribute Core.Text
mdbsaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE mdbsaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
--
-- /Note:/ Consider using 'valuesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaValuesToAdd :: Lens.Lens' ModifyDBSnapshotAttribute (Core.Maybe [Core.Text])
mdbsaValuesToAdd = Lens.field @"valuesToAdd"
{-# INLINEABLE mdbsaValuesToAdd #-}
{-# DEPRECATED valuesToAdd "Use generic-lens or generic-optics with 'valuesToAdd' instead"  #-}

-- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
--
-- /Note:/ Consider using 'valuesToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsaValuesToRemove :: Lens.Lens' ModifyDBSnapshotAttribute (Core.Maybe [Core.Text])
mdbsaValuesToRemove = Lens.field @"valuesToRemove"
{-# INLINEABLE mdbsaValuesToRemove #-}
{-# DEPRECATED valuesToRemove "Use generic-lens or generic-optics with 'valuesToRemove' instead"  #-}

instance Core.ToQuery ModifyDBSnapshotAttribute where
        toQuery ModifyDBSnapshotAttribute{..}
          = Core.toQueryPair "Action"
              ("ModifyDBSnapshotAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBSnapshotIdentifier" dBSnapshotIdentifier
              Core.<> Core.toQueryPair "AttributeName" attributeName
              Core.<>
              Core.toQueryPair "ValuesToAdd"
                (Core.maybe Core.mempty (Core.toQueryList "AttributeValue")
                   valuesToAdd)
              Core.<>
              Core.toQueryPair "ValuesToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "AttributeValue")
                   valuesToRemove)

instance Core.ToHeaders ModifyDBSnapshotAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyDBSnapshotAttribute where
        type Rs ModifyDBSnapshotAttribute =
             ModifyDBSnapshotAttributeResponse
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
          = Response.receiveXMLWrapper "ModifyDBSnapshotAttributeResult"
              (\ s h x ->
                 ModifyDBSnapshotAttributeResponse' Core.<$>
                   (x Core..@? "DBSnapshotAttributesResult") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyDBSnapshotAttributeResponse' smart constructor.
data ModifyDBSnapshotAttributeResponse = ModifyDBSnapshotAttributeResponse'
  { dBSnapshotAttributesResult :: Core.Maybe Types.DBSnapshotAttributesResult
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyDBSnapshotAttributeResponse' value with any optional fields omitted.
mkModifyDBSnapshotAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyDBSnapshotAttributeResponse
mkModifyDBSnapshotAttributeResponse responseStatus
  = ModifyDBSnapshotAttributeResponse'{dBSnapshotAttributesResult =
                                         Core.Nothing,
                                       responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsarrsDBSnapshotAttributesResult :: Lens.Lens' ModifyDBSnapshotAttributeResponse (Core.Maybe Types.DBSnapshotAttributesResult)
mdbsarrsDBSnapshotAttributesResult = Lens.field @"dBSnapshotAttributesResult"
{-# INLINEABLE mdbsarrsDBSnapshotAttributesResult #-}
{-# DEPRECATED dBSnapshotAttributesResult "Use generic-lens or generic-optics with 'dBSnapshotAttributesResult' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdbsarrsResponseStatus :: Lens.Lens' ModifyDBSnapshotAttributeResponse Core.Int
mdbsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mdbsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
