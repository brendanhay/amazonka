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
-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from a topic\'s access control policy.
module Network.AWS.SNS.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_topicArn,
    removePermission_label,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for RemovePermission action.
--
-- /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicArn :: Core.Text,
    -- | The unique label of the statement you want to remove.
    label :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'removePermission_topicArn' - The ARN of the topic whose access control policy you wish to modify.
--
-- 'label', 'removePermission_label' - The unique label of the statement you want to remove.
newRemovePermission ::
  -- | 'topicArn'
  Core.Text ->
  -- | 'label'
  Core.Text ->
  RemovePermission
newRemovePermission pTopicArn_ pLabel_ =
  RemovePermission'
    { topicArn = pTopicArn_,
      label = pLabel_
    }

-- | The ARN of the topic whose access control policy you wish to modify.
removePermission_topicArn :: Lens.Lens' RemovePermission Core.Text
removePermission_topicArn = Lens.lens (\RemovePermission' {topicArn} -> topicArn) (\s@RemovePermission' {} a -> s {topicArn = a} :: RemovePermission)

-- | The unique label of the statement you want to remove.
removePermission_label :: Lens.Lens' RemovePermission Core.Text
removePermission_label = Lens.lens (\RemovePermission' {label} -> label) (\s@RemovePermission' {} a -> s {label = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull RemovePermissionResponse'

instance Core.Hashable RemovePermission

instance Core.NFData RemovePermission

instance Core.ToHeaders RemovePermission where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RemovePermission where
  toPath = Core.const "/"

instance Core.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RemovePermission" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "TopicArn" Core.=: topicArn,
        "Label" Core.=: label
      ]

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Core.NFData RemovePermissionResponse
