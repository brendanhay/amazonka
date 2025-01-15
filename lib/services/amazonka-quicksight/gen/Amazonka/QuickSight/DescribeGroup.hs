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
-- Module      : Amazonka.QuickSight.DescribeGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an Amazon QuickSight group\'s description and Amazon Resource
-- Name (ARN).
module Amazonka.QuickSight.DescribeGroup
  ( -- * Creating a Request
    DescribeGroup (..),
    newDescribeGroup,

    -- * Request Lenses
    describeGroup_groupName,
    describeGroup_awsAccountId,
    describeGroup_namespace,

    -- * Destructuring the Response
    DescribeGroupResponse (..),
    newDescribeGroupResponse,

    -- * Response Lenses
    describeGroupResponse_group,
    describeGroupResponse_requestId,
    describeGroupResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { -- | The name of the group that you want to describe.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace of the group that you want described.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'describeGroup_groupName' - The name of the group that you want to describe.
--
-- 'awsAccountId', 'describeGroup_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'describeGroup_namespace' - The namespace of the group that you want described.
newDescribeGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeGroup
newDescribeGroup
  pGroupName_
  pAwsAccountId_
  pNamespace_ =
    DescribeGroup'
      { groupName = pGroupName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The name of the group that you want to describe.
describeGroup_groupName :: Lens.Lens' DescribeGroup Prelude.Text
describeGroup_groupName = Lens.lens (\DescribeGroup' {groupName} -> groupName) (\s@DescribeGroup' {} a -> s {groupName = a} :: DescribeGroup)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
describeGroup_awsAccountId :: Lens.Lens' DescribeGroup Prelude.Text
describeGroup_awsAccountId = Lens.lens (\DescribeGroup' {awsAccountId} -> awsAccountId) (\s@DescribeGroup' {} a -> s {awsAccountId = a} :: DescribeGroup)

-- | The namespace of the group that you want described.
describeGroup_namespace :: Lens.Lens' DescribeGroup Prelude.Text
describeGroup_namespace = Lens.lens (\DescribeGroup' {namespace} -> namespace) (\s@DescribeGroup' {} a -> s {namespace = a} :: DescribeGroup)

instance Core.AWSRequest DescribeGroup where
  type
    AWSResponse DescribeGroup =
      DescribeGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupResponse'
            Prelude.<$> (x Data..?> "Group")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGroup where
  hashWithSalt _salt DescribeGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeGroup where
  rnf DescribeGroup' {..} =
    Prelude.rnf groupName `Prelude.seq`
      Prelude.rnf awsAccountId `Prelude.seq`
        Prelude.rnf namespace

instance Data.ToHeaders DescribeGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGroup where
  toPath DescribeGroup' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/groups/",
        Data.toBS groupName
      ]

instance Data.ToQuery DescribeGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { -- | The name of the group.
    group' :: Prelude.Maybe Group,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'describeGroupResponse_group' - The name of the group.
--
-- 'requestId', 'describeGroupResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeGroupResponse_status' - The HTTP status of the request.
newDescribeGroupResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeGroupResponse
newDescribeGroupResponse pStatus_ =
  DescribeGroupResponse'
    { group' = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The name of the group.
describeGroupResponse_group :: Lens.Lens' DescribeGroupResponse (Prelude.Maybe Group)
describeGroupResponse_group = Lens.lens (\DescribeGroupResponse' {group'} -> group') (\s@DescribeGroupResponse' {} a -> s {group' = a} :: DescribeGroupResponse)

-- | The Amazon Web Services request ID for this operation.
describeGroupResponse_requestId :: Lens.Lens' DescribeGroupResponse (Prelude.Maybe Prelude.Text)
describeGroupResponse_requestId = Lens.lens (\DescribeGroupResponse' {requestId} -> requestId) (\s@DescribeGroupResponse' {} a -> s {requestId = a} :: DescribeGroupResponse)

-- | The HTTP status of the request.
describeGroupResponse_status :: Lens.Lens' DescribeGroupResponse Prelude.Int
describeGroupResponse_status = Lens.lens (\DescribeGroupResponse' {status} -> status) (\s@DescribeGroupResponse' {} a -> s {status = a} :: DescribeGroupResponse)

instance Prelude.NFData DescribeGroupResponse where
  rnf DescribeGroupResponse' {..} =
    Prelude.rnf group' `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf status
