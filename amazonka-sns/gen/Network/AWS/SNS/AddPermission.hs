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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicArn :: Prelude.Text,
    -- | A unique identifier for the new policy statement.
    label :: Prelude.Text,
    -- | The AWS account IDs of the users (principals) who will be given access
    -- to the specified actions. The users must have AWS accounts, but do not
    -- need to be signed up for this service.
    aWSAccountId :: [Prelude.Text],
    -- | The action you want to allow for the specified principal(s).
    --
    -- Valid values: Any Amazon SNS action name, for example @Publish@.
    actionName :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'label'
  Prelude.Text ->
  AddPermission
newAddPermission pTopicArn_ pLabel_ =
  AddPermission'
    { topicArn = pTopicArn_,
      label = pLabel_,
      aWSAccountId = Prelude.mempty,
      actionName = Prelude.mempty
    }

-- | The ARN of the topic whose access control policy you wish to modify.
addPermission_topicArn :: Lens.Lens' AddPermission Prelude.Text
addPermission_topicArn = Lens.lens (\AddPermission' {topicArn} -> topicArn) (\s@AddPermission' {} a -> s {topicArn = a} :: AddPermission)

-- | A unique identifier for the new policy statement.
addPermission_label :: Lens.Lens' AddPermission Prelude.Text
addPermission_label = Lens.lens (\AddPermission' {label} -> label) (\s@AddPermission' {} a -> s {label = a} :: AddPermission)

-- | The AWS account IDs of the users (principals) who will be given access
-- to the specified actions. The users must have AWS accounts, but do not
-- need to be signed up for this service.
addPermission_aWSAccountId :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_aWSAccountId = Lens.lens (\AddPermission' {aWSAccountId} -> aWSAccountId) (\s@AddPermission' {} a -> s {aWSAccountId = a} :: AddPermission) Prelude.. Prelude._Coerce

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@.
addPermission_actionName :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_actionName = Lens.lens (\AddPermission' {actionName} -> actionName) (\s@AddPermission' {} a -> s {actionName = a} :: AddPermission) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddPermissionResponse'

instance Prelude.Hashable AddPermission

instance Prelude.NFData AddPermission

instance Prelude.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AddPermission where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AddPermission" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Prelude.=: topicArn,
        "Label" Prelude.=: label,
        "AWSAccountId"
          Prelude.=: Prelude.toQueryList "member" aWSAccountId,
        "ActionName"
          Prelude.=: Prelude.toQueryList "member" actionName
      ]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddPermissionResponse ::
  AddPermissionResponse
newAddPermissionResponse = AddPermissionResponse'

instance Prelude.NFData AddPermissionResponse
