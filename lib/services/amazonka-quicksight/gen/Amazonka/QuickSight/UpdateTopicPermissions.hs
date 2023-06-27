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
-- Module      : Amazonka.QuickSight.UpdateTopicPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the permissions of a topic.
module Amazonka.QuickSight.UpdateTopicPermissions
  ( -- * Creating a Request
    UpdateTopicPermissions (..),
    newUpdateTopicPermissions,

    -- * Request Lenses
    updateTopicPermissions_grantPermissions,
    updateTopicPermissions_revokePermissions,
    updateTopicPermissions_awsAccountId,
    updateTopicPermissions_topicId,

    -- * Destructuring the Response
    UpdateTopicPermissionsResponse (..),
    newUpdateTopicPermissionsResponse,

    -- * Response Lenses
    updateTopicPermissionsResponse_permissions,
    updateTopicPermissionsResponse_requestId,
    updateTopicPermissionsResponse_topicArn,
    updateTopicPermissionsResponse_topicId,
    updateTopicPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTopicPermissions' smart constructor.
data UpdateTopicPermissions = UpdateTopicPermissions'
  { -- | The resource permissions that you want to grant to the topic.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | The resource permissions that you want to revoke from the topic.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
    -- | The ID of the Amazon Web Services account that contains the topic that
    -- you want to update the permissions for.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantPermissions', 'updateTopicPermissions_grantPermissions' - The resource permissions that you want to grant to the topic.
--
-- 'revokePermissions', 'updateTopicPermissions_revokePermissions' - The resource permissions that you want to revoke from the topic.
--
-- 'awsAccountId', 'updateTopicPermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the topic that
-- you want to update the permissions for.
--
-- 'topicId', 'updateTopicPermissions_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
newUpdateTopicPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  UpdateTopicPermissions
newUpdateTopicPermissions pAwsAccountId_ pTopicId_ =
  UpdateTopicPermissions'
    { grantPermissions =
        Prelude.Nothing,
      revokePermissions = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      topicId = pTopicId_
    }

-- | The resource permissions that you want to grant to the topic.
updateTopicPermissions_grantPermissions :: Lens.Lens' UpdateTopicPermissions (Prelude.Maybe [ResourcePermission])
updateTopicPermissions_grantPermissions = Lens.lens (\UpdateTopicPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateTopicPermissions' {} a -> s {grantPermissions = a} :: UpdateTopicPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The resource permissions that you want to revoke from the topic.
updateTopicPermissions_revokePermissions :: Lens.Lens' UpdateTopicPermissions (Prelude.Maybe [ResourcePermission])
updateTopicPermissions_revokePermissions = Lens.lens (\UpdateTopicPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateTopicPermissions' {} a -> s {revokePermissions = a} :: UpdateTopicPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the topic that
-- you want to update the permissions for.
updateTopicPermissions_awsAccountId :: Lens.Lens' UpdateTopicPermissions Prelude.Text
updateTopicPermissions_awsAccountId = Lens.lens (\UpdateTopicPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateTopicPermissions' {} a -> s {awsAccountId = a} :: UpdateTopicPermissions)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopicPermissions_topicId :: Lens.Lens' UpdateTopicPermissions Prelude.Text
updateTopicPermissions_topicId = Lens.lens (\UpdateTopicPermissions' {topicId} -> topicId) (\s@UpdateTopicPermissions' {} a -> s {topicId = a} :: UpdateTopicPermissions)

instance Core.AWSRequest UpdateTopicPermissions where
  type
    AWSResponse UpdateTopicPermissions =
      UpdateTopicPermissionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTopicPermissionsResponse'
            Prelude.<$> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TopicArn")
            Prelude.<*> (x Data..?> "TopicId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTopicPermissions where
  hashWithSalt _salt UpdateTopicPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData UpdateTopicPermissions where
  rnf UpdateTopicPermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId

instance Data.ToHeaders UpdateTopicPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTopicPermissions where
  toJSON UpdateTopicPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Data..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Data..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Data.ToPath UpdateTopicPermissions where
  toPath UpdateTopicPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/permissions"
      ]

instance Data.ToQuery UpdateTopicPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTopicPermissionsResponse' smart constructor.
data UpdateTopicPermissionsResponse = UpdateTopicPermissionsResponse'
  { -- | A list of resource permissions on the topic.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the topic.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the topic that you want to modify. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTopicPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'updateTopicPermissionsResponse_permissions' - A list of resource permissions on the topic.
--
-- 'requestId', 'updateTopicPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'topicArn', 'updateTopicPermissionsResponse_topicArn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'topicId', 'updateTopicPermissionsResponse_topicId' - The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'updateTopicPermissionsResponse_status' - The HTTP status of the request.
newUpdateTopicPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTopicPermissionsResponse
newUpdateTopicPermissionsResponse pStatus_ =
  UpdateTopicPermissionsResponse'
    { permissions =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      topicArn = Prelude.Nothing,
      topicId = Prelude.Nothing,
      status = pStatus_
    }

-- | A list of resource permissions on the topic.
updateTopicPermissionsResponse_permissions :: Lens.Lens' UpdateTopicPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateTopicPermissionsResponse_permissions = Lens.lens (\UpdateTopicPermissionsResponse' {permissions} -> permissions) (\s@UpdateTopicPermissionsResponse' {} a -> s {permissions = a} :: UpdateTopicPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
updateTopicPermissionsResponse_requestId :: Lens.Lens' UpdateTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
updateTopicPermissionsResponse_requestId = Lens.lens (\UpdateTopicPermissionsResponse' {requestId} -> requestId) (\s@UpdateTopicPermissionsResponse' {} a -> s {requestId = a} :: UpdateTopicPermissionsResponse)

-- | The Amazon Resource Name (ARN) of the topic.
updateTopicPermissionsResponse_topicArn :: Lens.Lens' UpdateTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
updateTopicPermissionsResponse_topicArn = Lens.lens (\UpdateTopicPermissionsResponse' {topicArn} -> topicArn) (\s@UpdateTopicPermissionsResponse' {} a -> s {topicArn = a} :: UpdateTopicPermissionsResponse)

-- | The ID of the topic that you want to modify. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateTopicPermissionsResponse_topicId :: Lens.Lens' UpdateTopicPermissionsResponse (Prelude.Maybe Prelude.Text)
updateTopicPermissionsResponse_topicId = Lens.lens (\UpdateTopicPermissionsResponse' {topicId} -> topicId) (\s@UpdateTopicPermissionsResponse' {} a -> s {topicId = a} :: UpdateTopicPermissionsResponse)

-- | The HTTP status of the request.
updateTopicPermissionsResponse_status :: Lens.Lens' UpdateTopicPermissionsResponse Prelude.Int
updateTopicPermissionsResponse_status = Lens.lens (\UpdateTopicPermissionsResponse' {status} -> status) (\s@UpdateTopicPermissionsResponse' {} a -> s {status = a} :: UpdateTopicPermissionsResponse)

instance
  Prelude.NFData
    UpdateTopicPermissionsResponse
  where
  rnf UpdateTopicPermissionsResponse' {..} =
    Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf status
