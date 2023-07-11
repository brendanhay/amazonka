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
-- Module      : Amazonka.SNS.RemovePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from a topic\'s access control policy.
--
-- To remove the ability to change topic permissions, you must deny
-- permissions to the @AddPermission@, @RemovePermission@, and
-- @SetTopicAttributes@ actions in your IAM policy.
module Amazonka.SNS.RemovePermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | Input for RemovePermission action.
--
-- /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicArn :: Prelude.Text,
    -- | The unique label of the statement you want to remove.
    label :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'label'
  Prelude.Text ->
  RemovePermission
newRemovePermission pTopicArn_ pLabel_ =
  RemovePermission'
    { topicArn = pTopicArn_,
      label = pLabel_
    }

-- | The ARN of the topic whose access control policy you wish to modify.
removePermission_topicArn :: Lens.Lens' RemovePermission Prelude.Text
removePermission_topicArn = Lens.lens (\RemovePermission' {topicArn} -> topicArn) (\s@RemovePermission' {} a -> s {topicArn = a} :: RemovePermission)

-- | The unique label of the statement you want to remove.
removePermission_label :: Lens.Lens' RemovePermission Prelude.Text
removePermission_label = Lens.lens (\RemovePermission' {label} -> label) (\s@RemovePermission' {} a -> s {label = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull RemovePermissionResponse'

instance Prelude.Hashable RemovePermission where
  hashWithSalt _salt RemovePermission' {..} =
    _salt
      `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` label

instance Prelude.NFData RemovePermission where
  rnf RemovePermission' {..} =
    Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf label

instance Data.ToHeaders RemovePermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemovePermission where
  toPath = Prelude.const "/"

instance Data.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemovePermission" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "TopicArn" Data.=: topicArn,
        "Label" Data.=: label
      ]

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Prelude.NFData RemovePermissionResponse where
  rnf _ = ()
