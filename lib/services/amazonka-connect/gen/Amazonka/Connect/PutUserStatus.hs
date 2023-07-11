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
-- Module      : Amazonka.Connect.PutUserStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the current status of a user or agent in Amazon Connect. If the
-- agent is currently handling a contact, this sets the agent\'s next
-- status.
--
-- For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/metrics-agent-status.html Agent status>
-- and
-- <https://docs.aws.amazon.com/connect/latest/adminguide/set-next-status.html Set your next status>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.PutUserStatus
  ( -- * Creating a Request
    PutUserStatus (..),
    newPutUserStatus,

    -- * Request Lenses
    putUserStatus_userId,
    putUserStatus_instanceId,
    putUserStatus_agentStatusId,

    -- * Destructuring the Response
    PutUserStatusResponse (..),
    newPutUserStatusResponse,

    -- * Response Lenses
    putUserStatusResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutUserStatus' smart constructor.
data PutUserStatus = PutUserStatus'
  { -- | The identifier of the user.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the agent status.
    agentStatusId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutUserStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'putUserStatus_userId' - The identifier of the user.
--
-- 'instanceId', 'putUserStatus_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'agentStatusId', 'putUserStatus_agentStatusId' - The identifier of the agent status.
newPutUserStatus ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'agentStatusId'
  Prelude.Text ->
  PutUserStatus
newPutUserStatus
  pUserId_
  pInstanceId_
  pAgentStatusId_ =
    PutUserStatus'
      { userId = pUserId_,
        instanceId = pInstanceId_,
        agentStatusId = pAgentStatusId_
      }

-- | The identifier of the user.
putUserStatus_userId :: Lens.Lens' PutUserStatus Prelude.Text
putUserStatus_userId = Lens.lens (\PutUserStatus' {userId} -> userId) (\s@PutUserStatus' {} a -> s {userId = a} :: PutUserStatus)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
putUserStatus_instanceId :: Lens.Lens' PutUserStatus Prelude.Text
putUserStatus_instanceId = Lens.lens (\PutUserStatus' {instanceId} -> instanceId) (\s@PutUserStatus' {} a -> s {instanceId = a} :: PutUserStatus)

-- | The identifier of the agent status.
putUserStatus_agentStatusId :: Lens.Lens' PutUserStatus Prelude.Text
putUserStatus_agentStatusId = Lens.lens (\PutUserStatus' {agentStatusId} -> agentStatusId) (\s@PutUserStatus' {} a -> s {agentStatusId = a} :: PutUserStatus)

instance Core.AWSRequest PutUserStatus where
  type
    AWSResponse PutUserStatus =
      PutUserStatusResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutUserStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutUserStatus where
  hashWithSalt _salt PutUserStatus' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` agentStatusId

instance Prelude.NFData PutUserStatus where
  rnf PutUserStatus' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf agentStatusId

instance Data.ToHeaders PutUserStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutUserStatus where
  toJSON PutUserStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AgentStatusId" Data..= agentStatusId)
          ]
      )

instance Data.ToPath PutUserStatus where
  toPath PutUserStatus' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId,
        "/status"
      ]

instance Data.ToQuery PutUserStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutUserStatusResponse' smart constructor.
data PutUserStatusResponse = PutUserStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutUserStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putUserStatusResponse_httpStatus' - The response's http status code.
newPutUserStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutUserStatusResponse
newPutUserStatusResponse pHttpStatus_ =
  PutUserStatusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putUserStatusResponse_httpStatus :: Lens.Lens' PutUserStatusResponse Prelude.Int
putUserStatusResponse_httpStatus = Lens.lens (\PutUserStatusResponse' {httpStatus} -> httpStatus) (\s@PutUserStatusResponse' {} a -> s {httpStatus = a} :: PutUserStatusResponse)

instance Prelude.NFData PutUserStatusResponse where
  rnf PutUserStatusResponse' {..} =
    Prelude.rnf httpStatus
