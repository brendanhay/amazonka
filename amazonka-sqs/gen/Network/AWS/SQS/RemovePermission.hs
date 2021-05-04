{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes any permissions in the queue policy that matches the specified
-- @Label@ parameter.
--
-- -   Only the owner of a queue can remove permissions from it.
--
-- -   Cross-account permissions don\'t apply to this action. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-customer-managed-policy-examples.html#grant-cross-account-permissions-to-role-and-user-name Grant cross-account permissions to a role and a user name>
--     in the /Amazon Simple Queue Service Developer Guide/.
--
-- -   To remove the ability to change queue permissions, you must deny
--     permission to the @AddPermission@, @RemovePermission@, and
--     @SetQueueAttributes@ actions in your IAM policy.
module Network.AWS.SQS.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_queueUrl,
    removePermission_label,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The URL of the Amazon SQS queue from which permissions are removed.
    --
    -- Queue URLs and names are case-sensitive.
    queueUrl :: Prelude.Text,
    -- | The identification of the permission to remove. This is the label added
    -- using the @ AddPermission @ action.
    label :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueUrl', 'removePermission_queueUrl' - The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
--
-- 'label', 'removePermission_label' - The identification of the permission to remove. This is the label added
-- using the @ AddPermission @ action.
newRemovePermission ::
  -- | 'queueUrl'
  Prelude.Text ->
  -- | 'label'
  Prelude.Text ->
  RemovePermission
newRemovePermission pQueueUrl_ pLabel_ =
  RemovePermission'
    { queueUrl = pQueueUrl_,
      label = pLabel_
    }

-- | The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
removePermission_queueUrl :: Lens.Lens' RemovePermission Prelude.Text
removePermission_queueUrl = Lens.lens (\RemovePermission' {queueUrl} -> queueUrl) (\s@RemovePermission' {} a -> s {queueUrl = a} :: RemovePermission)

-- | The identification of the permission to remove. This is the label added
-- using the @ AddPermission @ action.
removePermission_label :: Lens.Lens' RemovePermission Prelude.Text
removePermission_label = Lens.lens (\RemovePermission' {label} -> label) (\s@RemovePermission' {} a -> s {label = a} :: RemovePermission)

instance Prelude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull RemovePermissionResponse'

instance Prelude.Hashable RemovePermission

instance Prelude.NFData RemovePermission

instance Prelude.ToHeaders RemovePermission where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RemovePermission where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RemovePermission" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-11-05" :: Prelude.ByteString),
        "QueueUrl" Prelude.=: queueUrl,
        "Label" Prelude.=: label
      ]

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Prelude.NFData RemovePermissionResponse
