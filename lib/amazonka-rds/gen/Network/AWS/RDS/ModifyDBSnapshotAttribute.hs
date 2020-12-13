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
    mdsaDBSnapshotIdentifier,
    mdsaValuesToAdd,
    mdsaValuesToRemove,
    mdsaAttributeName,

    -- * Destructuring the response
    ModifyDBSnapshotAttributeResponse (..),
    mkModifyDBSnapshotAttributeResponse,

    -- ** Response lenses
    mdsarsDBSnapshotAttributesResult,
    mdsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyDBSnapshotAttribute' smart constructor.
data ModifyDBSnapshotAttribute = ModifyDBSnapshotAttribute'
  { -- | The identifier for the DB snapshot to modify the attributes for.
    dbSnapshotIdentifier :: Lude.Text,
    -- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
    --
    -- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
    valuesToAdd :: Lude.Maybe [Lude.Text],
    -- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
    --
    -- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
    valuesToRemove :: Lude.Maybe [Lude.Text],
    -- | The name of the DB snapshot attribute to modify.
    --
    -- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
    attributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBSnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'dbSnapshotIdentifier' - The identifier for the DB snapshot to modify the attributes for.
-- * 'valuesToAdd' - A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
-- * 'valuesToRemove' - A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
-- * 'attributeName' - The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
mkModifyDBSnapshotAttribute ::
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  ModifyDBSnapshotAttribute
mkModifyDBSnapshotAttribute pDBSnapshotIdentifier_ pAttributeName_ =
  ModifyDBSnapshotAttribute'
    { dbSnapshotIdentifier =
        pDBSnapshotIdentifier_,
      valuesToAdd = Lude.Nothing,
      valuesToRemove = Lude.Nothing,
      attributeName = pAttributeName_
    }

-- | The identifier for the DB snapshot to modify the attributes for.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsaDBSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshotAttribute Lude.Text
mdsaDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: ModifyDBSnapshotAttribute -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: ModifyDBSnapshotAttribute)
{-# DEPRECATED mdsaDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

-- | A list of DB snapshot attributes to add to the attribute specified by @AttributeName@ .
--
-- To authorize other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account IDs, or @all@ to make the manual DB snapshot restorable by any AWS account. Do not add the @all@ value for any manual DB snapshots that contain private information that you don't want available to all AWS accounts.
--
-- /Note:/ Consider using 'valuesToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsaValuesToAdd :: Lens.Lens' ModifyDBSnapshotAttribute (Lude.Maybe [Lude.Text])
mdsaValuesToAdd = Lens.lens (valuesToAdd :: ModifyDBSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {valuesToAdd = a} :: ModifyDBSnapshotAttribute)
{-# DEPRECATED mdsaValuesToAdd "Use generic-lens or generic-optics with 'valuesToAdd' instead." #-}

-- | A list of DB snapshot attributes to remove from the attribute specified by @AttributeName@ .
--
-- To remove authorization for other AWS accounts to copy or restore a manual snapshot, set this list to include one or more AWS account identifiers, or @all@ to remove authorization for any AWS account to copy or restore the DB snapshot. If you specify @all@ , an AWS account whose account ID is explicitly added to the @restore@ attribute can still copy or restore the manual DB snapshot.
--
-- /Note:/ Consider using 'valuesToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsaValuesToRemove :: Lens.Lens' ModifyDBSnapshotAttribute (Lude.Maybe [Lude.Text])
mdsaValuesToRemove = Lens.lens (valuesToRemove :: ModifyDBSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {valuesToRemove = a} :: ModifyDBSnapshotAttribute)
{-# DEPRECATED mdsaValuesToRemove "Use generic-lens or generic-optics with 'valuesToRemove' instead." #-}

-- | The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a manual DB snapshot, set this value to @restore@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsaAttributeName :: Lens.Lens' ModifyDBSnapshotAttribute Lude.Text
mdsaAttributeName = Lens.lens (attributeName :: ModifyDBSnapshotAttribute -> Lude.Text) (\s a -> s {attributeName = a} :: ModifyDBSnapshotAttribute)
{-# DEPRECATED mdsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest ModifyDBSnapshotAttribute where
  type
    Rs ModifyDBSnapshotAttribute =
      ModifyDBSnapshotAttributeResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBSnapshotAttributeResponse'
            Lude.<$> (x Lude..@? "DBSnapshotAttributesResult")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBSnapshotAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBSnapshotAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBSnapshotAttribute where
  toQuery ModifyDBSnapshotAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBSnapshotAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier,
        "ValuesToAdd"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AttributeValue" Lude.<$> valuesToAdd),
        "ValuesToRemove"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AttributeValue" Lude.<$> valuesToRemove),
        "AttributeName" Lude.=: attributeName
      ]

-- | /See:/ 'mkModifyDBSnapshotAttributeResponse' smart constructor.
data ModifyDBSnapshotAttributeResponse = ModifyDBSnapshotAttributeResponse'
  { dbSnapshotAttributesResult :: Lude.Maybe DBSnapshotAttributesResult,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBSnapshotAttributeResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshotAttributesResult' -
-- * 'responseStatus' - The response status code.
mkModifyDBSnapshotAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBSnapshotAttributeResponse
mkModifyDBSnapshotAttributeResponse pResponseStatus_ =
  ModifyDBSnapshotAttributeResponse'
    { dbSnapshotAttributesResult =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsarsDBSnapshotAttributesResult :: Lens.Lens' ModifyDBSnapshotAttributeResponse (Lude.Maybe DBSnapshotAttributesResult)
mdsarsDBSnapshotAttributesResult = Lens.lens (dbSnapshotAttributesResult :: ModifyDBSnapshotAttributeResponse -> Lude.Maybe DBSnapshotAttributesResult) (\s a -> s {dbSnapshotAttributesResult = a} :: ModifyDBSnapshotAttributeResponse)
{-# DEPRECATED mdsarsDBSnapshotAttributesResult "Use generic-lens or generic-optics with 'dbSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdsarsResponseStatus :: Lens.Lens' ModifyDBSnapshotAttributeResponse Lude.Int
mdsarsResponseStatus = Lens.lens (responseStatus :: ModifyDBSnapshotAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBSnapshotAttributeResponse)
{-# DEPRECATED mdsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
