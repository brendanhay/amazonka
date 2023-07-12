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
-- Module      : Amazonka.SNS.AddPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic\'s access control policy, granting access
-- for the specified Amazon Web Services accounts to the specified actions.
--
-- To remove the ability to change topic permissions, you must deny
-- permissions to the @AddPermission@, @RemovePermission@, and
-- @SetTopicAttributes@ actions in your IAM policy.
module Amazonka.SNS.AddPermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicArn :: Prelude.Text,
    -- | A unique identifier for the new policy statement.
    label :: Prelude.Text,
    -- | The Amazon Web Services account IDs of the users (principals) who will
    -- be given access to the specified actions. The users must have Amazon Web
    -- Services account, but do not need to be signed up for this service.
    aWSAccountId :: [Prelude.Text],
    -- | The action you want to allow for the specified principal(s).
    --
    -- Valid values: Any Amazon SNS action name, for example @Publish@.
    actionName :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'aWSAccountId', 'addPermission_aWSAccountId' - The Amazon Web Services account IDs of the users (principals) who will
-- be given access to the specified actions. The users must have Amazon Web
-- Services account, but do not need to be signed up for this service.
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

-- | The Amazon Web Services account IDs of the users (principals) who will
-- be given access to the specified actions. The users must have Amazon Web
-- Services account, but do not need to be signed up for this service.
addPermission_aWSAccountId :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_aWSAccountId = Lens.lens (\AddPermission' {aWSAccountId} -> aWSAccountId) (\s@AddPermission' {} a -> s {aWSAccountId = a} :: AddPermission) Prelude.. Lens.coerced

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@.
addPermission_actionName :: Lens.Lens' AddPermission [Prelude.Text]
addPermission_actionName = Lens.lens (\AddPermission' {actionName} -> actionName) (\s@AddPermission' {} a -> s {actionName = a} :: AddPermission) Prelude.. Lens.coerced

instance Core.AWSRequest AddPermission where
  type
    AWSResponse AddPermission =
      AddPermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull AddPermissionResponse'

instance Prelude.Hashable AddPermission where
  hashWithSalt _salt AddPermission' {..} =
    _salt
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` aWSAccountId
      `Prelude.hashWithSalt` actionName

instance Prelude.NFData AddPermission where
  rnf AddPermission' {..} =
    Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf aWSAccountId
      `Prelude.seq` Prelude.rnf actionName

instance Data.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AddPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AddPermission" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Data.=: topicArn,
        "Label" Data.=: label,
        "AWSAccountId"
          Data.=: Data.toQueryList "member" aWSAccountId,
        "ActionName"
          Data.=: Data.toQueryList "member" actionName
      ]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddPermissionResponse ::
  AddPermissionResponse
newAddPermissionResponse = AddPermissionResponse'

instance Prelude.NFData AddPermissionResponse where
  rnf _ = ()
