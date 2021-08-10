{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSnapshotAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values
-- from, a manual DB snapshot.
--
-- To share a manual DB snapshot with other AWS accounts, specify @restore@
-- as the @AttributeName@ and use the @ValuesToAdd@ parameter to add a list
-- of IDs of the AWS accounts that are authorized to restore the manual DB
-- snapshot. Uses the value @all@ to make the manual DB snapshot public,
-- which means it can be copied or restored by all AWS accounts.
--
-- Don\'t add the @all@ value for any manual DB snapshots that contain
-- private information that you don\'t want available to all AWS accounts.
--
-- If the manual DB snapshot is encrypted, it can be shared, but only by
-- specifying a list of authorized AWS account IDs for the @ValuesToAdd@
-- parameter. You can\'t use @all@ as a value for that parameter in this
-- case.
--
-- To view which AWS accounts have access to copy or restore a manual DB
-- snapshot, or whether a manual DB snapshot public or private, use the
-- DescribeDBSnapshotAttributes API action. The accounts are returned as
-- values for the @restore@ attribute.
module Network.AWS.RDS.ModifyDBSnapshotAttribute
  ( -- * Creating a Request
    ModifyDBSnapshotAttribute (..),
    newModifyDBSnapshotAttribute,

    -- * Request Lenses
    modifyDBSnapshotAttribute_valuesToRemove,
    modifyDBSnapshotAttribute_valuesToAdd,
    modifyDBSnapshotAttribute_dbSnapshotIdentifier,
    modifyDBSnapshotAttribute_attributeName,

    -- * Destructuring the Response
    ModifyDBSnapshotAttributeResponse (..),
    newModifyDBSnapshotAttributeResponse,

    -- * Response Lenses
    modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult,
    modifyDBSnapshotAttributeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyDBSnapshotAttribute' smart constructor.
data ModifyDBSnapshotAttribute = ModifyDBSnapshotAttribute'
  { -- | A list of DB snapshot attributes to remove from the attribute specified
    -- by @AttributeName@.
    --
    -- To remove authorization for other AWS accounts to copy or restore a
    -- manual snapshot, set this list to include one or more AWS account
    -- identifiers, or @all@ to remove authorization for any AWS account to
    -- copy or restore the DB snapshot. If you specify @all@, an AWS account
    -- whose account ID is explicitly added to the @restore@ attribute can
    -- still copy or restore the manual DB snapshot.
    valuesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | A list of DB snapshot attributes to add to the attribute specified by
    -- @AttributeName@.
    --
    -- To authorize other AWS accounts to copy or restore a manual snapshot,
    -- set this list to include one or more AWS account IDs, or @all@ to make
    -- the manual DB snapshot restorable by any AWS account. Do not add the
    -- @all@ value for any manual DB snapshots that contain private information
    -- that you don\'t want available to all AWS accounts.
    valuesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The identifier for the DB snapshot to modify the attributes for.
    dbSnapshotIdentifier :: Prelude.Text,
    -- | The name of the DB snapshot attribute to modify.
    --
    -- To manage authorization for other AWS accounts to copy or restore a
    -- manual DB snapshot, set this value to @restore@.
    --
    -- To view the list of attributes available to modify, use the
    -- DescribeDBSnapshotAttributes API action.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valuesToRemove', 'modifyDBSnapshotAttribute_valuesToRemove' - A list of DB snapshot attributes to remove from the attribute specified
-- by @AttributeName@.
--
-- To remove authorization for other AWS accounts to copy or restore a
-- manual snapshot, set this list to include one or more AWS account
-- identifiers, or @all@ to remove authorization for any AWS account to
-- copy or restore the DB snapshot. If you specify @all@, an AWS account
-- whose account ID is explicitly added to the @restore@ attribute can
-- still copy or restore the manual DB snapshot.
--
-- 'valuesToAdd', 'modifyDBSnapshotAttribute_valuesToAdd' - A list of DB snapshot attributes to add to the attribute specified by
-- @AttributeName@.
--
-- To authorize other AWS accounts to copy or restore a manual snapshot,
-- set this list to include one or more AWS account IDs, or @all@ to make
-- the manual DB snapshot restorable by any AWS account. Do not add the
-- @all@ value for any manual DB snapshots that contain private information
-- that you don\'t want available to all AWS accounts.
--
-- 'dbSnapshotIdentifier', 'modifyDBSnapshotAttribute_dbSnapshotIdentifier' - The identifier for the DB snapshot to modify the attributes for.
--
-- 'attributeName', 'modifyDBSnapshotAttribute_attributeName' - The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a
-- manual DB snapshot, set this value to @restore@.
--
-- To view the list of attributes available to modify, use the
-- DescribeDBSnapshotAttributes API action.
newModifyDBSnapshotAttribute ::
  -- | 'dbSnapshotIdentifier'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  ModifyDBSnapshotAttribute
newModifyDBSnapshotAttribute
  pDBSnapshotIdentifier_
  pAttributeName_ =
    ModifyDBSnapshotAttribute'
      { valuesToRemove =
          Prelude.Nothing,
        valuesToAdd = Prelude.Nothing,
        dbSnapshotIdentifier = pDBSnapshotIdentifier_,
        attributeName = pAttributeName_
      }

-- | A list of DB snapshot attributes to remove from the attribute specified
-- by @AttributeName@.
--
-- To remove authorization for other AWS accounts to copy or restore a
-- manual snapshot, set this list to include one or more AWS account
-- identifiers, or @all@ to remove authorization for any AWS account to
-- copy or restore the DB snapshot. If you specify @all@, an AWS account
-- whose account ID is explicitly added to the @restore@ attribute can
-- still copy or restore the manual DB snapshot.
modifyDBSnapshotAttribute_valuesToRemove :: Lens.Lens' ModifyDBSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBSnapshotAttribute_valuesToRemove = Lens.lens (\ModifyDBSnapshotAttribute' {valuesToRemove} -> valuesToRemove) (\s@ModifyDBSnapshotAttribute' {} a -> s {valuesToRemove = a} :: ModifyDBSnapshotAttribute) Prelude.. Lens.mapping Lens._Coerce

-- | A list of DB snapshot attributes to add to the attribute specified by
-- @AttributeName@.
--
-- To authorize other AWS accounts to copy or restore a manual snapshot,
-- set this list to include one or more AWS account IDs, or @all@ to make
-- the manual DB snapshot restorable by any AWS account. Do not add the
-- @all@ value for any manual DB snapshots that contain private information
-- that you don\'t want available to all AWS accounts.
modifyDBSnapshotAttribute_valuesToAdd :: Lens.Lens' ModifyDBSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBSnapshotAttribute_valuesToAdd = Lens.lens (\ModifyDBSnapshotAttribute' {valuesToAdd} -> valuesToAdd) (\s@ModifyDBSnapshotAttribute' {} a -> s {valuesToAdd = a} :: ModifyDBSnapshotAttribute) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier for the DB snapshot to modify the attributes for.
modifyDBSnapshotAttribute_dbSnapshotIdentifier :: Lens.Lens' ModifyDBSnapshotAttribute Prelude.Text
modifyDBSnapshotAttribute_dbSnapshotIdentifier = Lens.lens (\ModifyDBSnapshotAttribute' {dbSnapshotIdentifier} -> dbSnapshotIdentifier) (\s@ModifyDBSnapshotAttribute' {} a -> s {dbSnapshotIdentifier = a} :: ModifyDBSnapshotAttribute)

-- | The name of the DB snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a
-- manual DB snapshot, set this value to @restore@.
--
-- To view the list of attributes available to modify, use the
-- DescribeDBSnapshotAttributes API action.
modifyDBSnapshotAttribute_attributeName :: Lens.Lens' ModifyDBSnapshotAttribute Prelude.Text
modifyDBSnapshotAttribute_attributeName = Lens.lens (\ModifyDBSnapshotAttribute' {attributeName} -> attributeName) (\s@ModifyDBSnapshotAttribute' {} a -> s {attributeName = a} :: ModifyDBSnapshotAttribute)

instance Core.AWSRequest ModifyDBSnapshotAttribute where
  type
    AWSResponse ModifyDBSnapshotAttribute =
      ModifyDBSnapshotAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBSnapshotAttributeResponse'
            Prelude.<$> (x Core..@? "DBSnapshotAttributesResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBSnapshotAttribute

instance Prelude.NFData ModifyDBSnapshotAttribute

instance Core.ToHeaders ModifyDBSnapshotAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBSnapshotAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBSnapshotAttribute where
  toQuery ModifyDBSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBSnapshotAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ValuesToRemove"
          Core.=: Core.toQuery
            ( Core.toQueryList "AttributeValue"
                Prelude.<$> valuesToRemove
            ),
        "ValuesToAdd"
          Core.=: Core.toQuery
            ( Core.toQueryList "AttributeValue"
                Prelude.<$> valuesToAdd
            ),
        "DBSnapshotIdentifier" Core.=: dbSnapshotIdentifier,
        "AttributeName" Core.=: attributeName
      ]

-- | /See:/ 'newModifyDBSnapshotAttributeResponse' smart constructor.
data ModifyDBSnapshotAttributeResponse = ModifyDBSnapshotAttributeResponse'
  { dbSnapshotAttributesResult :: Prelude.Maybe DBSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBSnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSnapshotAttributesResult', 'modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult' - Undocumented member.
--
-- 'httpStatus', 'modifyDBSnapshotAttributeResponse_httpStatus' - The response's http status code.
newModifyDBSnapshotAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBSnapshotAttributeResponse
newModifyDBSnapshotAttributeResponse pHttpStatus_ =
  ModifyDBSnapshotAttributeResponse'
    { dbSnapshotAttributesResult =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult :: Lens.Lens' ModifyDBSnapshotAttributeResponse (Prelude.Maybe DBSnapshotAttributesResult)
modifyDBSnapshotAttributeResponse_dbSnapshotAttributesResult = Lens.lens (\ModifyDBSnapshotAttributeResponse' {dbSnapshotAttributesResult} -> dbSnapshotAttributesResult) (\s@ModifyDBSnapshotAttributeResponse' {} a -> s {dbSnapshotAttributesResult = a} :: ModifyDBSnapshotAttributeResponse)

-- | The response's http status code.
modifyDBSnapshotAttributeResponse_httpStatus :: Lens.Lens' ModifyDBSnapshotAttributeResponse Prelude.Int
modifyDBSnapshotAttributeResponse_httpStatus = Lens.lens (\ModifyDBSnapshotAttributeResponse' {httpStatus} -> httpStatus) (\s@ModifyDBSnapshotAttributeResponse' {} a -> s {httpStatus = a} :: ModifyDBSnapshotAttributeResponse)

instance
  Prelude.NFData
    ModifyDBSnapshotAttributeResponse
