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
-- Module      : Amazonka.QuickSight.DescribeTopicPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions of a topic.
module Amazonka.QuickSight.DescribeTopicPermissions
  ( -- * Creating a Request
    DescribeTopicPermissions (..),
    newDescribeTopicPermissions,

    -- * Request Lenses
    describeTopicPermissions_awsAccountId,
    describeTopicPermissions_topicId,

    -- * Destructuring the Response
    DescribeTopicPermissionsResponse (..),
    newDescribeTopicPermissionsResponse,

    -- * Response Lenses
    describeTopicPermissionsResponse_permissions,
    describeTopicPermissionsResponse_requestId,
    describeTopicPermissionsResponse_topicArn,
    describeTopicPermissionsResponse_topicId,
    describeTopicPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTopicPermissions' smart constructor.
data DescribeTopicPermissions = DescribeTopicPermissions'
  { -- | The ID of the Amazon Web Services account that contains the topic that
    -- you want described.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTopicPermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the topic that
-- you want described.
--
-- 'topicId', 'describeTopicPermissions_topicId' - The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newDescribeTopicPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  DescribeTopicPermissions
newDescribeTopicPermissions pAwsAccountId_ pTopicId_ =
  DescribeTopicPermissions'
    { awsAccountId =
        pAwsAccountId_,
      topicId = pTopicId_
    }

-- | The ID of the Amazon Web Services account that contains the topic that
-- you want described.
describeTopicPermissions_awsAccountId :: Lens.Lens' DescribeTopicPermissions Prelude.Text
describeTopicPermissions_awsAccountId = Lens.lens (\DescribeTopicPermissions' {awsAccountId} -> awsAccountId) (\s@DescribeTopicPermissions' {} a -> s {awsAccountId = a} :: DescribeTopicPermissions)

-- | The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeTopicPermissions_topicId :: Lens.Lens' DescribeTopicPermissions Prelude.Text
describeTopicPermissions_topicId = Lens.lens (\DescribeTopicPermissions' {topicId} -> topicId) (\s@DescribeTopicPermissions' {} a -> s {topicId = a} :: DescribeTopicPermissions)

instance Core.AWSRequest DescribeTopicPermissions where
  type
    AWSResponse DescribeTopicPermissions =
      DescribeTopicPermissionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicPermissionsResponse'
            Prelude.<$> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTopicPermissions where
  hashWithSalt _salt DescribeTopicPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData DescribeTopicPermissions where
  rnf DescribeTopicPermissions' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId

instance Data.ToHeaders DescribeTopicPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTopicPermissions where
  toPath DescribeTopicPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/permissions"
      ]

instance Data.ToQuery DescribeTopicPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTopicPermissionsResponse' smart constructor.
data DescribeTopicPermissionsResponse = DescribeTopicPermissionsResponse'
  { -- | A list of resource permissions that are configured to the topic.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'describeTopicPermissionsResponse_permissions' - A list of resource permissions that are configured to the topic.
--
-- 'requestId', 'describeTopicPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'describeTopicPermissionsResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'describeTopicPermissionsResponse_topicId' - The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'describeTopicPermissionsResponse_status' - The HTTP status of the request.
newDescribeTopicPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTopicPermissionsResponse
newDescribeTopicPermissionsResponse pStatus_ =
  DescribeTopicPermissionsResponse'
    { permissions =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of resource permissions that are configured to the topic.
describeTopicPermissionsResponse_permissions :: Lens.Lens' DescribeTopicPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
describeTopicPermissionsResponse_permissions = Lens.lens (\DescribeTopicPermissionsResponse' {permissions} -> permissions) (\s@DescribeTopicPermissionsResponse' {} a -> s {permissions = a} :: DescribeTopicPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
describeTopicPermissionsResponse_requestId :: Lens.Lens' DescribeTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
describeTopicPermissionsResponse_requestId = Lens.lens (\DescribeTopicPermissionsResponse' {requestId} -> requestId) (\s@DescribeTopicPermissionsResponse' {} a -> s {requestId = a} :: DescribeTopicPermissionsResponse)

-- | The Amazon Resource Name (ARN) of the topic.
describeTopicPermissionsResponse_topicArn :: Lens.Lens' DescribeTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
describeTopicPermissionsResponse_topicArn = Lens.lens (\DescribeTopicPermissionsResponse' {topicArn} -> topicArn) (\s@DescribeTopicPermissionsResponse' {} a -> s {topicArn = a} :: DescribeTopicPermissionsResponse)

-- | The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeTopicPermissionsResponse_topicId :: Lens.Lens' DescribeTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
describeTopicPermissionsResponse_topicId = Lens.lens (\DescribeTopicPermissionsResponse' {topicId} -> topicId) (\s@DescribeTopicPermissionsResponse' {} a -> s {topicId = a} :: DescribeTopicPermissionsResponse)

-- | The HTTP status of the request.
describeTopicPermissionsResponse_status :: Lens.Lens' DescribeTopicPermissionsResponse Prelude.Int
describeTopicPermissionsResponse_status = Lens.lens (\DescribeTopicPermissionsResponse' {status} -> status) (\s@DescribeTopicPermissionsResponse' {} a -> s {status = a} :: DescribeTopicPermissionsResponse)

instance
  Prelude.NFData
    DescribeTopicPermissionsResponse
  where
  rnf DescribeTopicPermissionsResponse' {..} =
    Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status
