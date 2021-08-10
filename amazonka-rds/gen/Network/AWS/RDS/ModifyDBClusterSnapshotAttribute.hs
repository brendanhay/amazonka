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
-- Module      : Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values
-- from, a manual DB cluster snapshot.
--
-- To share a manual DB cluster snapshot with other AWS accounts, specify
-- @restore@ as the @AttributeName@ and use the @ValuesToAdd@ parameter to
-- add a list of IDs of the AWS accounts that are authorized to restore the
-- manual DB cluster snapshot. Use the value @all@ to make the manual DB
-- cluster snapshot public, which means that it can be copied or restored
-- by all AWS accounts.
--
-- Don\'t add the @all@ value for any manual DB cluster snapshots that
-- contain private information that you don\'t want available to all AWS
-- accounts.
--
-- If a manual DB cluster snapshot is encrypted, it can be shared, but only
-- by specifying a list of authorized AWS account IDs for the @ValuesToAdd@
-- parameter. You can\'t use @all@ as a value for that parameter in this
-- case.
--
-- To view which AWS accounts have access to copy or restore a manual DB
-- cluster snapshot, or whether a manual DB cluster snapshot is public or
-- private, use the DescribeDBClusterSnapshotAttributes API action. The
-- accounts are returned as values for the @restore@ attribute.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.ModifyDBClusterSnapshotAttribute
  ( -- * Creating a Request
    ModifyDBClusterSnapshotAttribute (..),
    newModifyDBClusterSnapshotAttribute,

    -- * Request Lenses
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,

    -- * Destructuring the Response
    ModifyDBClusterSnapshotAttributeResponse (..),
    newModifyDBClusterSnapshotAttributeResponse,

    -- * Response Lenses
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,
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
-- /See:/ 'newModifyDBClusterSnapshotAttribute' smart constructor.
data ModifyDBClusterSnapshotAttribute = ModifyDBClusterSnapshotAttribute'
  { -- | A list of DB cluster snapshot attributes to remove from the attribute
    -- specified by @AttributeName@.
    --
    -- To remove authorization for other AWS accounts to copy or restore a
    -- manual DB cluster snapshot, set this list to include one or more AWS
    -- account identifiers, or @all@ to remove authorization for any AWS
    -- account to copy or restore the DB cluster snapshot. If you specify
    -- @all@, an AWS account whose account ID is explicitly added to the
    -- @restore@ attribute can still copy or restore a manual DB cluster
    -- snapshot.
    valuesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | A list of DB cluster snapshot attributes to add to the attribute
    -- specified by @AttributeName@.
    --
    -- To authorize other AWS accounts to copy or restore a manual DB cluster
    -- snapshot, set this list to include one or more AWS account IDs, or @all@
    -- to make the manual DB cluster snapshot restorable by any AWS account. Do
    -- not add the @all@ value for any manual DB cluster snapshots that contain
    -- private information that you don\'t want available to all AWS accounts.
    valuesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The identifier for the DB cluster snapshot to modify the attributes for.
    dbClusterSnapshotIdentifier :: Prelude.Text,
    -- | The name of the DB cluster snapshot attribute to modify.
    --
    -- To manage authorization for other AWS accounts to copy or restore a
    -- manual DB cluster snapshot, set this value to @restore@.
    --
    -- To view the list of attributes available to modify, use the
    -- DescribeDBClusterSnapshotAttributes API action.
    attributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBClusterSnapshotAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valuesToRemove', 'modifyDBClusterSnapshotAttribute_valuesToRemove' - A list of DB cluster snapshot attributes to remove from the attribute
-- specified by @AttributeName@.
--
-- To remove authorization for other AWS accounts to copy or restore a
-- manual DB cluster snapshot, set this list to include one or more AWS
-- account identifiers, or @all@ to remove authorization for any AWS
-- account to copy or restore the DB cluster snapshot. If you specify
-- @all@, an AWS account whose account ID is explicitly added to the
-- @restore@ attribute can still copy or restore a manual DB cluster
-- snapshot.
--
-- 'valuesToAdd', 'modifyDBClusterSnapshotAttribute_valuesToAdd' - A list of DB cluster snapshot attributes to add to the attribute
-- specified by @AttributeName@.
--
-- To authorize other AWS accounts to copy or restore a manual DB cluster
-- snapshot, set this list to include one or more AWS account IDs, or @all@
-- to make the manual DB cluster snapshot restorable by any AWS account. Do
-- not add the @all@ value for any manual DB cluster snapshots that contain
-- private information that you don\'t want available to all AWS accounts.
--
-- 'dbClusterSnapshotIdentifier', 'modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to modify the attributes for.
--
-- 'attributeName', 'modifyDBClusterSnapshotAttribute_attributeName' - The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a
-- manual DB cluster snapshot, set this value to @restore@.
--
-- To view the list of attributes available to modify, use the
-- DescribeDBClusterSnapshotAttributes API action.
newModifyDBClusterSnapshotAttribute ::
  -- | 'dbClusterSnapshotIdentifier'
  Prelude.Text ->
  -- | 'attributeName'
  Prelude.Text ->
  ModifyDBClusterSnapshotAttribute
newModifyDBClusterSnapshotAttribute
  pDBClusterSnapshotIdentifier_
  pAttributeName_ =
    ModifyDBClusterSnapshotAttribute'
      { valuesToRemove =
          Prelude.Nothing,
        valuesToAdd = Prelude.Nothing,
        dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_,
        attributeName = pAttributeName_
      }

-- | A list of DB cluster snapshot attributes to remove from the attribute
-- specified by @AttributeName@.
--
-- To remove authorization for other AWS accounts to copy or restore a
-- manual DB cluster snapshot, set this list to include one or more AWS
-- account identifiers, or @all@ to remove authorization for any AWS
-- account to copy or restore the DB cluster snapshot. If you specify
-- @all@, an AWS account whose account ID is explicitly added to the
-- @restore@ attribute can still copy or restore a manual DB cluster
-- snapshot.
modifyDBClusterSnapshotAttribute_valuesToRemove :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBClusterSnapshotAttribute_valuesToRemove = Lens.lens (\ModifyDBClusterSnapshotAttribute' {valuesToRemove} -> valuesToRemove) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {valuesToRemove = a} :: ModifyDBClusterSnapshotAttribute) Prelude.. Lens.mapping Lens._Coerce

-- | A list of DB cluster snapshot attributes to add to the attribute
-- specified by @AttributeName@.
--
-- To authorize other AWS accounts to copy or restore a manual DB cluster
-- snapshot, set this list to include one or more AWS account IDs, or @all@
-- to make the manual DB cluster snapshot restorable by any AWS account. Do
-- not add the @all@ value for any manual DB cluster snapshots that contain
-- private information that you don\'t want available to all AWS accounts.
modifyDBClusterSnapshotAttribute_valuesToAdd :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBClusterSnapshotAttribute_valuesToAdd = Lens.lens (\ModifyDBClusterSnapshotAttribute' {valuesToAdd} -> valuesToAdd) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {valuesToAdd = a} :: ModifyDBClusterSnapshotAttribute) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier for the DB cluster snapshot to modify the attributes for.
modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier :: Lens.Lens' ModifyDBClusterSnapshotAttribute Prelude.Text
modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier = Lens.lens (\ModifyDBClusterSnapshotAttribute' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {dbClusterSnapshotIdentifier = a} :: ModifyDBClusterSnapshotAttribute)

-- | The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other AWS accounts to copy or restore a
-- manual DB cluster snapshot, set this value to @restore@.
--
-- To view the list of attributes available to modify, use the
-- DescribeDBClusterSnapshotAttributes API action.
modifyDBClusterSnapshotAttribute_attributeName :: Lens.Lens' ModifyDBClusterSnapshotAttribute Prelude.Text
modifyDBClusterSnapshotAttribute_attributeName = Lens.lens (\ModifyDBClusterSnapshotAttribute' {attributeName} -> attributeName) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {attributeName = a} :: ModifyDBClusterSnapshotAttribute)

instance
  Core.AWSRequest
    ModifyDBClusterSnapshotAttribute
  where
  type
    AWSResponse ModifyDBClusterSnapshotAttribute =
      ModifyDBClusterSnapshotAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBClusterSnapshotAttributeResponse'
            Prelude.<$> (x Core..@? "DBClusterSnapshotAttributesResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyDBClusterSnapshotAttribute

instance
  Prelude.NFData
    ModifyDBClusterSnapshotAttribute

instance
  Core.ToHeaders
    ModifyDBClusterSnapshotAttribute
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBClusterSnapshotAttribute where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ModifyDBClusterSnapshotAttribute
  where
  toQuery ModifyDBClusterSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyDBClusterSnapshotAttribute" ::
                      Prelude.ByteString
                  ),
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
        "DBClusterSnapshotIdentifier"
          Core.=: dbClusterSnapshotIdentifier,
        "AttributeName" Core.=: attributeName
      ]

-- | /See:/ 'newModifyDBClusterSnapshotAttributeResponse' smart constructor.
data ModifyDBClusterSnapshotAttributeResponse = ModifyDBClusterSnapshotAttributeResponse'
  { dbClusterSnapshotAttributesResult :: Prelude.Maybe DBClusterSnapshotAttributesResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBClusterSnapshotAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterSnapshotAttributesResult', 'modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult' - Undocumented member.
--
-- 'httpStatus', 'modifyDBClusterSnapshotAttributeResponse_httpStatus' - The response's http status code.
newModifyDBClusterSnapshotAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBClusterSnapshotAttributeResponse
newModifyDBClusterSnapshotAttributeResponse
  pHttpStatus_ =
    ModifyDBClusterSnapshotAttributeResponse'
      { dbClusterSnapshotAttributesResult =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse (Prelude.Maybe DBClusterSnapshotAttributesResult)
modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult = Lens.lens (\ModifyDBClusterSnapshotAttributeResponse' {dbClusterSnapshotAttributesResult} -> dbClusterSnapshotAttributesResult) (\s@ModifyDBClusterSnapshotAttributeResponse' {} a -> s {dbClusterSnapshotAttributesResult = a} :: ModifyDBClusterSnapshotAttributeResponse)

-- | The response's http status code.
modifyDBClusterSnapshotAttributeResponse_httpStatus :: Lens.Lens' ModifyDBClusterSnapshotAttributeResponse Prelude.Int
modifyDBClusterSnapshotAttributeResponse_httpStatus = Lens.lens (\ModifyDBClusterSnapshotAttributeResponse' {httpStatus} -> httpStatus) (\s@ModifyDBClusterSnapshotAttributeResponse' {} a -> s {httpStatus = a} :: ModifyDBClusterSnapshotAttributeResponse)

instance
  Prelude.NFData
    ModifyDBClusterSnapshotAttributeResponse
