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
-- Module      : Amazonka.QuickSight.CreateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @CreateGroup@ operation to create a group in Amazon QuickSight.
-- You can create up to 10,000 groups in a namespace. If you want to create
-- more than 10,000 groups in a namespace, contact AWS Support.
--
-- The permissions resource is
-- @arn:aws:quicksight:\<your-region>:@/@\<relevant-aws-account-id>@/@:group\/default\/@/@\<group-name>@/@ @.
--
-- The response is a group object.
module Amazonka.QuickSight.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_description,
    createGroup_groupName,
    createGroup_awsAccountId,
    createGroup_namespace,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_group,
    createGroupResponse_requestId,
    createGroupResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for this operation.
--
-- /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | A description for the group that you want to create.
    description :: Prelude.Maybe Prelude.Text,
    -- | A name for the group that you want to create.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace that you want the group to be a part of.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createGroup_description' - A description for the group that you want to create.
--
-- 'groupName', 'createGroup_groupName' - A name for the group that you want to create.
--
-- 'awsAccountId', 'createGroup_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'createGroup_namespace' - The namespace that you want the group to be a part of.
newCreateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  CreateGroup
newCreateGroup pGroupName_ pAwsAccountId_ pNamespace_ =
  CreateGroup'
    { description = Prelude.Nothing,
      groupName = pGroupName_,
      awsAccountId = pAwsAccountId_,
      namespace = pNamespace_
    }

-- | A description for the group that you want to create.
createGroup_description :: Lens.Lens' CreateGroup (Prelude.Maybe Prelude.Text)
createGroup_description = Lens.lens (\CreateGroup' {description} -> description) (\s@CreateGroup' {} a -> s {description = a} :: CreateGroup)

-- | A name for the group that you want to create.
createGroup_groupName :: Lens.Lens' CreateGroup Prelude.Text
createGroup_groupName = Lens.lens (\CreateGroup' {groupName} -> groupName) (\s@CreateGroup' {} a -> s {groupName = a} :: CreateGroup)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
createGroup_awsAccountId :: Lens.Lens' CreateGroup Prelude.Text
createGroup_awsAccountId = Lens.lens (\CreateGroup' {awsAccountId} -> awsAccountId) (\s@CreateGroup' {} a -> s {awsAccountId = a} :: CreateGroup)

-- | The namespace that you want the group to be a part of.
createGroup_namespace :: Lens.Lens' CreateGroup Prelude.Text
createGroup_namespace = Lens.lens (\CreateGroup' {namespace} -> namespace) (\s@CreateGroup' {} a -> s {namespace = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroup where
  hashWithSalt _salt CreateGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData CreateGroup where
  rnf CreateGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf groupName `Prelude.seq`
        Prelude.rnf awsAccountId `Prelude.seq`
          Prelude.rnf namespace

instance Data.ToHeaders CreateGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("GroupName" Data..= groupName)
          ]
      )

instance Data.ToPath CreateGroup where
  toPath CreateGroup' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/groups"
      ]

instance Data.ToQuery CreateGroup where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for this operation.
--
-- /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The name of the group.
    group' :: Prelude.Maybe Group,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'createGroupResponse_group' - The name of the group.
--
-- 'requestId', 'createGroupResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createGroupResponse_status' - The HTTP status of the request.
newCreateGroupResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateGroupResponse
newCreateGroupResponse pStatus_ =
  CreateGroupResponse'
    { group' = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The name of the group.
createGroupResponse_group :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Group)
createGroupResponse_group = Lens.lens (\CreateGroupResponse' {group'} -> group') (\s@CreateGroupResponse' {} a -> s {group' = a} :: CreateGroupResponse)

-- | The Amazon Web Services request ID for this operation.
createGroupResponse_requestId :: Lens.Lens' CreateGroupResponse (Prelude.Maybe Prelude.Text)
createGroupResponse_requestId = Lens.lens (\CreateGroupResponse' {requestId} -> requestId) (\s@CreateGroupResponse' {} a -> s {requestId = a} :: CreateGroupResponse)

-- | The HTTP status of the request.
createGroupResponse_status :: Lens.Lens' CreateGroupResponse Prelude.Int
createGroupResponse_status = Lens.lens (\CreateGroupResponse' {status} -> status) (\s@CreateGroupResponse' {} a -> s {status = a} :: CreateGroupResponse)

instance Prelude.NFData CreateGroupResponse where
  rnf CreateGroupResponse' {..} =
    Prelude.rnf group' `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf status
