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
-- Module      : Amazonka.QuickSight.UpdateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes a group description.
module Amazonka.QuickSight.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_description,
    updateGroup_groupName,
    updateGroup_awsAccountId,
    updateGroup_namespace,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_group,
    updateGroupResponse_requestId,
    updateGroupResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The description for the group that you want to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the group that you want to update.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace of the group that you want to update.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateGroup_description' - The description for the group that you want to update.
--
-- 'groupName', 'updateGroup_groupName' - The name of the group that you want to update.
--
-- 'awsAccountId', 'updateGroup_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'updateGroup_namespace' - The namespace of the group that you want to update.
newUpdateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  UpdateGroup
newUpdateGroup pGroupName_ pAwsAccountId_ pNamespace_ =
  UpdateGroup'
    { description = Prelude.Nothing,
      groupName = pGroupName_,
      awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | The description for the group that you want to update.
updateGroup_description :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_description = Lens.lens (\UpdateGroup' {description} -> description) (\s@UpdateGroup' {} a -> s {description = a} :: UpdateGroup)

-- | The name of the group that you want to update.
updateGroup_groupName :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
updateGroup_awsAccountId :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_awsAccountId = Lens.lens (\UpdateGroup' {awsAccountId} -> awsAccountId) (\s@UpdateGroup' {} a -> s {awsAccountId = a} :: UpdateGroup)

-- | The namespace of the group that you want to update.
updateGroup_namespace :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_namespace = Lens.lens (\UpdateGroup' {namespace} -> namespace) (\s@UpdateGroup' {} a -> s {namespace = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGroup where
  hashWithSalt _salt UpdateGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData UpdateGroup where
  rnf UpdateGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf groupName `Prelude.seq`
        Prelude.rnf awsAccountId `Prelude.seq`
          Prelude.rnf namespace

instance Data.ToHeaders UpdateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateGroup where
  toPath UpdateGroup' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/groups/",
        Data.toBS groupName
      ]

instance Data.ToQuery UpdateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The name of the group.
    group' :: Prelude.Maybe Group,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'updateGroupResponse_group' - The name of the group.
--
-- 'requestId', 'updateGroupResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateGroupResponse_status' - The HTTP status of the request.
newUpdateGroupResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pStatus_ =
  UpdateGroupResponse'
    { group' = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The name of the group.
updateGroupResponse_group :: Lens.Lens' UpdateGroupResponse (Prelude.Maybe Group)
updateGroupResponse_group = Lens.lens (\UpdateGroupResponse' {group'} -> group') (\s@UpdateGroupResponse' {} a -> s {group' = a} :: UpdateGroupResponse)

-- | The Amazon Web Services request ID for this operation.
updateGroupResponse_requestId :: Lens.Lens' UpdateGroupResponse (Prelude.Maybe Prelude.Text)
updateGroupResponse_requestId = Lens.lens (\UpdateGroupResponse' {requestId} -> requestId) (\s@UpdateGroupResponse' {} a -> s {requestId = a} :: UpdateGroupResponse)

-- | The HTTP status of the request.
updateGroupResponse_status :: Lens.Lens' UpdateGroupResponse Prelude.Int
updateGroupResponse_status = Lens.lens (\UpdateGroupResponse' {status} -> status) (\s@UpdateGroupResponse' {} a -> s {status = a} :: UpdateGroupResponse)

instance Prelude.NFData UpdateGroupResponse where
  rnf UpdateGroupResponse' {..} =
    Prelude.rnf group' `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf status
