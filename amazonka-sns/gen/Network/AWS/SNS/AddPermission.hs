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
-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic\'s access control policy, granting access
-- for the specified AWS accounts to the specified actions.
module Network.AWS.SNS.AddPermission
  ( -- * Creating a Request
    AddPermission (..),
    newAddPermission,

    -- * Request Lenses
    addPermission_topicArn,
    addPermission_label,
    addPermission_aWSAccountId,
    addPermission_actionName,

    -- * Destructuring the Response
    AddPermissionResponse (..),
    newAddPermissionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicArn :: Core.Text,
    -- | A unique identifier for the new policy statement.
    label :: Core.Text,
    -- | The AWS account IDs of the users (principals) who will be given access
    -- to the specified actions. The users must have AWS accounts, but do not
    -- need to be signed up for this service.
    aWSAccountId :: [Core.Text],
    -- | The action you want to allow for the specified principal(s).
    --
    -- Valid values: Any Amazon SNS action name, for example @Publish@.
    actionName :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'addPermission_topicArn' - The ARN of the topic whose access control policy you wish to modify.
--
-- 'label', 'addPermission_label' - A unique identifier for the new policy statement.
--
-- 'aWSAccountId', 'addPermission_aWSAccountId' - The AWS account IDs of the users (principals) who will be given access
-- to the specified actions. The users must have AWS accounts, but do not
-- need to be signed up for this service.
--
-- 'actionName', 'addPermission_actionName' - The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@.
newAddPermission ::
  -- | 'topicArn'
  Core.Text ->
  -- | 'label'
  Core.Text ->
  AddPermission
newAddPermission pTopicArn_ pLabel_ =
  AddPermission'
    { topicArn = pTopicArn_,
      label = pLabel_,
      aWSAccountId = Core.mempty,
      actionName = Core.mempty
    }

-- | The ARN of the topic whose access control policy you wish to modify.
addPermission_topicArn :: Lens.Lens' AddPermission Core.Text
addPermission_topicArn = Lens.lens (\AddPermission' {topicArn} -> topicArn) (\s@AddPermission' {} a -> s {topicArn = a} :: AddPermission)

-- | A unique identifier for the new policy statement.
addPermission_label :: Lens.Lens' AddPermission Core.Text
addPermission_label = Lens.lens (\AddPermission' {label} -> label) (\s@AddPermission' {} a -> s {label = a} :: AddPermission)

-- | The AWS account IDs of the users (principals) who will be given access
-- to the specified actions. The users must have AWS accounts, but do not
-- need to be signed up for this service.
addPermission_aWSAccountId :: Lens.Lens' AddPermission [Core.Text]
addPermission_aWSAccountId = Lens.lens (\AddPermission' {aWSAccountId} -> aWSAccountId) (\s@AddPermission' {} a -> s {aWSAccountId = a} :: AddPermission) Core.. Lens._Coerce

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@.
addPermission_actionName :: Lens.Lens' AddPermission [Core.Text]
addPermission_actionName = Lens.lens (\AddPermission' {actionName} -> actionName) (\s@AddPermission' {} a -> s {actionName = a} :: AddPermission) Core.. Lens._Coerce

instance Core.AWSRequest AddPermission where
  type
    AWSResponse AddPermission =
      AddPermissionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddPermissionResponse'

instance Core.Hashable AddPermission

instance Core.NFData AddPermission

instance Core.ToHeaders AddPermission where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AddPermission where
  toPath = Core.const "/"

instance Core.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AddPermission" :: Core.ByteString),
        "Version" Core.=: ("2010-03-31" :: Core.ByteString),
        "TopicArn" Core.=: topicArn,
        "Label" Core.=: label,
        "AWSAccountId"
          Core.=: Core.toQueryList "member" aWSAccountId,
        "ActionName"
          Core.=: Core.toQueryList "member" actionName
      ]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddPermissionResponse ::
  AddPermissionResponse
newAddPermissionResponse = AddPermissionResponse'

instance Core.NFData AddPermissionResponse
