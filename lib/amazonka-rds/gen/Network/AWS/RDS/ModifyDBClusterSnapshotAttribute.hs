{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    mdcsaValuesToAdd,
    mdcsaValuesToRemove,
    mdcsaDBClusterSnapshotIdentifier,
    mdcsaAttributeName,

    -- * Destructuring the response
    ModifyDBClusterSnapshotAttributeResponse (..),
    mkModifyDBClusterSnapshotAttributeResponse,

    -- ** Response lenses
    mdcsarsDBClusterSnapshotAttributesResult,
    mdcsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBClusterSnapshotAttribute' smart constructor.
data ModifyDBClusterSnapshotAttribute = ModifyDBClusterSnapshotAttribute'
  { valuesToAdd ::
      Lude.Maybe [Lude.Text],
    valuesToRemove ::
      Lude.Maybe [Lude.Text],
    dbClusterSnapshotIdentifier ::
      Lude.Text,
    attributeName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
-- * 'dbClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to modify the attributes for.
-- * 'valuesToAdd' - A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
-- * 'valuesToRemove' - A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
mkModifyDBClusterSnapshotAttribute ::
  -- | 'dbClusterSnapshotIdentifier'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  ModifyDBClusterSnapshotAttribute
mkModifyDBClusterSnapshotAttribute
  pDBClusterSnapshotIdentifier_
  pAttributeName_ =
    ModifyDBClusterSnapshotAttribute'
      { valuesToAdd = Lude.Nothing,
        valuesToRemove = Lude.Nothing,
        dbClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_,
        attributeName = pAttributeName_
      }

-- | A list of DB cluster snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB cluster snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB cluster snapshots that contain private information that you don't want available to all AWS accounts.
--
-- /Note:/ Consider using 'valuesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsaValuesToAdd :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Lude.Maybe [Lude.Text])
mdcsaValuesToAdd = Lens.lens (valuesToAdd :: ModifyDBClusterSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {valuesToAdd = a} :: ModifyDBClusterSnapshotAttribute)
{-# DEPRECATED mdcsaValuesToAdd "Use generic-lens or generic-optics with 'valuesToAdd' instead." #-}

-- | A list of DB cluster snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB cluster snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore a manual DB cluster snapshot.
--
-- /Note:/ Consider using 'valuesToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsaValuesToRemove :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Lude.Maybe [Lude.Text])
mdcsaValuesToRemove = Lens.lens (valuesToRemove :: ModifyDBClusterSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {valuesToRemove = a} :: ModifyDBClusterSnapshotAttribute)
{-# DEPRECATED mdcsaValuesToRemove "Use generic-lens or generic-optics with 'valuesToRemove' instead." #-}

-- | The identifier for the DB cluster snapshot to modify the attributes for.
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsaDBClusterSnapshotIdentifier :: Lens.Lens' ModifyDBClusterSnapshotAttribute Lude.Text
mdcsaDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: ModifyDBClusterSnapshotAttribute -> Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: ModifyDBClusterSnapshotAttribute)
{-# DEPRECATED mdcsaDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

-- | The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB cluster snapshot, set this value to @restore@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsaAttributeName :: Lens.Lens' ModifyDBClusterSnapshotAttribute Lude.Text
mdcsaAttributeName = Lens.lens (attributeName :: ModifyDBClusterSnapshotAttribute -> Lude.Text) (\s a -> s {attributeName = a} :: ModifyDBClusterSnapshotAttribute)
{-# DEPRECATED mdcsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest ModifyDBClusterSnapshotAttribute where
  type
    Rs ModifyDBClusterSnapshotAttribute =
      ModifyDBClusterSnapshotAttributeResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBClusterSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBClusterSnapshotAttributeResponse'
            Lude.<$> (x Lude..@? "DBClusterSnapshotAttributesResult")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBClusterSnapshotAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBClusterSnapshotAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBClusterSnapshotAttribute where
  toQuery ModifyDBClusterSnapshotAttribute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyDBClusterSnapshotAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ValuesToAdd"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AttributeValue" Lude.<$> valuesToAdd),
        "ValuesToRemove"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AttributeValue" Lude.<$> valuesToRemove),
        "DBClusterSnapshotIdentifier" Lude.=: dbClusterSnapshotIdentifier,
        "AttributeName" Lude.=: attributeName
      ]

-- | /See:/ 'mkModifyDBClusterSnapshotAttributeResponse' smart constructor.
data ModifyDBClusterSnapshotAttributeResponse = ModifyDBClusterSnapshotAttributeResponse'
  { dbClusterSnapshotAttributesResult ::
      Lude.Maybe
        DBClusterSnapshotAttributesResult,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBClusterSnapshotAttributeResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshotAttributesResult' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyDBClusterSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBClusterSnapshotAttributeResponse
mkModifyDBClusterSnapshotAttributeResponse pResponseStatus_ =
  ModifyDBClusterSnapshotAttributeResponse'
    { dbClusterSnapshotAttributesResult =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsarsDBClusterSnapshotAttributesResult :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse (Lude.Maybe DBClusterSnapshotAttributesResult)
mdcsarsDBClusterSnapshotAttributesResult = Lens.lens (dbClusterSnapshotAttributesResult :: ModifyDBClusterSnapshotAttributeResponse -> Lude.Maybe DBClusterSnapshotAttributesResult) (\s a -> s {dbClusterSnapshotAttributesResult = a} :: ModifyDBClusterSnapshotAttributeResponse)
{-# DEPRECATED mdcsarsDBClusterSnapshotAttributesResult "Use generic-lens or generic-optics with 'dbClusterSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsarsResponseStatus :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse Lude.Int
mdcsarsResponseStatus = Lens.lens (responseStatus :: ModifyDBClusterSnapshotAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBClusterSnapshotAttributeResponse)
{-# DEPRECATED mdcsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
