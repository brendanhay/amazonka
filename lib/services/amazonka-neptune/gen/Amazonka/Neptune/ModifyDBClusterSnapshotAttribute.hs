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
-- Module      : Amazonka.Neptune.ModifyDBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an attribute and values to, or removes an attribute and values
-- from, a manual DB cluster snapshot.
--
-- To share a manual DB cluster snapshot with other Amazon accounts,
-- specify @restore@ as the @AttributeName@ and use the @ValuesToAdd@
-- parameter to add a list of IDs of the Amazon accounts that are
-- authorized to restore the manual DB cluster snapshot. Use the value
-- @all@ to make the manual DB cluster snapshot public, which means that it
-- can be copied or restored by all Amazon accounts. Do not add the @all@
-- value for any manual DB cluster snapshots that contain private
-- information that you don\'t want available to all Amazon accounts. If a
-- manual DB cluster snapshot is encrypted, it can be shared, but only by
-- specifying a list of authorized Amazon account IDs for the @ValuesToAdd@
-- parameter. You can\'t use @all@ as a value for that parameter in this
-- case.
--
-- To view which Amazon accounts have access to copy or restore a manual DB
-- cluster snapshot, or whether a manual DB cluster snapshot public or
-- private, use the DescribeDBClusterSnapshotAttributes API action.
module Amazonka.Neptune.ModifyDBClusterSnapshotAttribute
  ( -- * Creating a Request
    ModifyDBClusterSnapshotAttribute (..),
    newModifyDBClusterSnapshotAttribute,

    -- * Request Lenses
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_valuesToRemove,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyDBClusterSnapshotAttribute' smart constructor.
data ModifyDBClusterSnapshotAttribute = ModifyDBClusterSnapshotAttribute'
  { -- | A list of DB cluster snapshot attributes to add to the attribute
    -- specified by @AttributeName@.
    --
    -- To authorize other Amazon accounts to copy or restore a manual DB
    -- cluster snapshot, set this list to include one or more Amazon account
    -- IDs, or @all@ to make the manual DB cluster snapshot restorable by any
    -- Amazon account. Do not add the @all@ value for any manual DB cluster
    -- snapshots that contain private information that you don\'t want
    -- available to all Amazon accounts.
    valuesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | A list of DB cluster snapshot attributes to remove from the attribute
    -- specified by @AttributeName@.
    --
    -- To remove authorization for other Amazon accounts to copy or restore a
    -- manual DB cluster snapshot, set this list to include one or more Amazon
    -- account identifiers, or @all@ to remove authorization for any Amazon
    -- account to copy or restore the DB cluster snapshot. If you specify
    -- @all@, an Amazon account whose account ID is explicitly added to the
    -- @restore@ attribute can still copy or restore a manual DB cluster
    -- snapshot.
    valuesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The identifier for the DB cluster snapshot to modify the attributes for.
    dbClusterSnapshotIdentifier :: Prelude.Text,
    -- | The name of the DB cluster snapshot attribute to modify.
    --
    -- To manage authorization for other Amazon accounts to copy or restore a
    -- manual DB cluster snapshot, set this value to @restore@.
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
-- 'valuesToAdd', 'modifyDBClusterSnapshotAttribute_valuesToAdd' - A list of DB cluster snapshot attributes to add to the attribute
-- specified by @AttributeName@.
--
-- To authorize other Amazon accounts to copy or restore a manual DB
-- cluster snapshot, set this list to include one or more Amazon account
-- IDs, or @all@ to make the manual DB cluster snapshot restorable by any
-- Amazon account. Do not add the @all@ value for any manual DB cluster
-- snapshots that contain private information that you don\'t want
-- available to all Amazon accounts.
--
-- 'valuesToRemove', 'modifyDBClusterSnapshotAttribute_valuesToRemove' - A list of DB cluster snapshot attributes to remove from the attribute
-- specified by @AttributeName@.
--
-- To remove authorization for other Amazon accounts to copy or restore a
-- manual DB cluster snapshot, set this list to include one or more Amazon
-- account identifiers, or @all@ to remove authorization for any Amazon
-- account to copy or restore the DB cluster snapshot. If you specify
-- @all@, an Amazon account whose account ID is explicitly added to the
-- @restore@ attribute can still copy or restore a manual DB cluster
-- snapshot.
--
-- 'dbClusterSnapshotIdentifier', 'modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to modify the attributes for.
--
-- 'attributeName', 'modifyDBClusterSnapshotAttribute_attributeName' - The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other Amazon accounts to copy or restore a
-- manual DB cluster snapshot, set this value to @restore@.
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
      { valuesToAdd =
          Prelude.Nothing,
        valuesToRemove = Prelude.Nothing,
        dbClusterSnapshotIdentifier =
          pDBClusterSnapshotIdentifier_,
        attributeName = pAttributeName_
      }

-- | A list of DB cluster snapshot attributes to add to the attribute
-- specified by @AttributeName@.
--
-- To authorize other Amazon accounts to copy or restore a manual DB
-- cluster snapshot, set this list to include one or more Amazon account
-- IDs, or @all@ to make the manual DB cluster snapshot restorable by any
-- Amazon account. Do not add the @all@ value for any manual DB cluster
-- snapshots that contain private information that you don\'t want
-- available to all Amazon accounts.
modifyDBClusterSnapshotAttribute_valuesToAdd :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBClusterSnapshotAttribute_valuesToAdd = Lens.lens (\ModifyDBClusterSnapshotAttribute' {valuesToAdd} -> valuesToAdd) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {valuesToAdd = a} :: ModifyDBClusterSnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | A list of DB cluster snapshot attributes to remove from the attribute
-- specified by @AttributeName@.
--
-- To remove authorization for other Amazon accounts to copy or restore a
-- manual DB cluster snapshot, set this list to include one or more Amazon
-- account identifiers, or @all@ to remove authorization for any Amazon
-- account to copy or restore the DB cluster snapshot. If you specify
-- @all@, an Amazon account whose account ID is explicitly added to the
-- @restore@ attribute can still copy or restore a manual DB cluster
-- snapshot.
modifyDBClusterSnapshotAttribute_valuesToRemove :: Lens.Lens' ModifyDBClusterSnapshotAttribute (Prelude.Maybe [Prelude.Text])
modifyDBClusterSnapshotAttribute_valuesToRemove = Lens.lens (\ModifyDBClusterSnapshotAttribute' {valuesToRemove} -> valuesToRemove) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {valuesToRemove = a} :: ModifyDBClusterSnapshotAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the DB cluster snapshot to modify the attributes for.
modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier :: Lens.Lens' ModifyDBClusterSnapshotAttribute Prelude.Text
modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier = Lens.lens (\ModifyDBClusterSnapshotAttribute' {dbClusterSnapshotIdentifier} -> dbClusterSnapshotIdentifier) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {dbClusterSnapshotIdentifier = a} :: ModifyDBClusterSnapshotAttribute)

-- | The name of the DB cluster snapshot attribute to modify.
--
-- To manage authorization for other Amazon accounts to copy or restore a
-- manual DB cluster snapshot, set this value to @restore@.
modifyDBClusterSnapshotAttribute_attributeName :: Lens.Lens' ModifyDBClusterSnapshotAttribute Prelude.Text
modifyDBClusterSnapshotAttribute_attributeName = Lens.lens (\ModifyDBClusterSnapshotAttribute' {attributeName} -> attributeName) (\s@ModifyDBClusterSnapshotAttribute' {} a -> s {attributeName = a} :: ModifyDBClusterSnapshotAttribute)

instance
  Core.AWSRequest
    ModifyDBClusterSnapshotAttribute
  where
  type
    AWSResponse ModifyDBClusterSnapshotAttribute =
      ModifyDBClusterSnapshotAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterSnapshotAttributeResult"
      ( \s h x ->
          ModifyDBClusterSnapshotAttributeResponse'
            Prelude.<$> (x Data..@? "DBClusterSnapshotAttributesResult")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyDBClusterSnapshotAttribute
  where
  hashWithSalt
    _salt
    ModifyDBClusterSnapshotAttribute' {..} =
      _salt `Prelude.hashWithSalt` valuesToAdd
        `Prelude.hashWithSalt` valuesToRemove
        `Prelude.hashWithSalt` dbClusterSnapshotIdentifier
        `Prelude.hashWithSalt` attributeName

instance
  Prelude.NFData
    ModifyDBClusterSnapshotAttribute
  where
  rnf ModifyDBClusterSnapshotAttribute' {..} =
    Prelude.rnf valuesToAdd
      `Prelude.seq` Prelude.rnf valuesToRemove
      `Prelude.seq` Prelude.rnf dbClusterSnapshotIdentifier
      `Prelude.seq` Prelude.rnf attributeName

instance
  Data.ToHeaders
    ModifyDBClusterSnapshotAttribute
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBClusterSnapshotAttribute where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyDBClusterSnapshotAttribute
  where
  toQuery ModifyDBClusterSnapshotAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyDBClusterSnapshotAttribute" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ValuesToAdd"
          Data.=: Data.toQuery
            ( Data.toQueryList "AttributeValue"
                Prelude.<$> valuesToAdd
            ),
        "ValuesToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "AttributeValue"
                Prelude.<$> valuesToRemove
            ),
        "DBClusterSnapshotIdentifier"
          Data.=: dbClusterSnapshotIdentifier,
        "AttributeName" Data.=: attributeName
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
  where
  rnf ModifyDBClusterSnapshotAttributeResponse' {..} =
    Prelude.rnf dbClusterSnapshotAttributesResult
      `Prelude.seq` Prelude.rnf httpStatus
