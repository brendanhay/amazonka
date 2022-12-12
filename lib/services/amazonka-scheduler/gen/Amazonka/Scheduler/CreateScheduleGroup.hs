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
-- Module      : Amazonka.Scheduler.CreateScheduleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified schedule group.
module Amazonka.Scheduler.CreateScheduleGroup
  ( -- * Creating a Request
    CreateScheduleGroup (..),
    newCreateScheduleGroup,

    -- * Request Lenses
    createScheduleGroup_clientToken,
    createScheduleGroup_tags,
    createScheduleGroup_name,

    -- * Destructuring the Response
    CreateScheduleGroupResponse (..),
    newCreateScheduleGroupResponse,

    -- * Response Lenses
    createScheduleGroupResponse_httpStatus,
    createScheduleGroupResponse_scheduleGroupArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newCreateScheduleGroup' smart constructor.
data CreateScheduleGroup = CreateScheduleGroup'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, EventBridge
    -- Scheduler uses a randomly generated token for the request to ensure
    -- idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The list of tags to associate with the schedule group.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the schedule group that you are creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createScheduleGroup_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
--
-- 'tags', 'createScheduleGroup_tags' - The list of tags to associate with the schedule group.
--
-- 'name', 'createScheduleGroup_name' - The name of the schedule group that you are creating.
newCreateScheduleGroup ::
  -- | 'name'
  Prelude.Text ->
  CreateScheduleGroup
newCreateScheduleGroup pName_ =
  CreateScheduleGroup'
    { clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
createScheduleGroup_clientToken :: Lens.Lens' CreateScheduleGroup (Prelude.Maybe Prelude.Text)
createScheduleGroup_clientToken = Lens.lens (\CreateScheduleGroup' {clientToken} -> clientToken) (\s@CreateScheduleGroup' {} a -> s {clientToken = a} :: CreateScheduleGroup)

-- | The list of tags to associate with the schedule group.
createScheduleGroup_tags :: Lens.Lens' CreateScheduleGroup (Prelude.Maybe [Tag])
createScheduleGroup_tags = Lens.lens (\CreateScheduleGroup' {tags} -> tags) (\s@CreateScheduleGroup' {} a -> s {tags = a} :: CreateScheduleGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the schedule group that you are creating.
createScheduleGroup_name :: Lens.Lens' CreateScheduleGroup Prelude.Text
createScheduleGroup_name = Lens.lens (\CreateScheduleGroup' {name} -> name) (\s@CreateScheduleGroup' {} a -> s {name = a} :: CreateScheduleGroup)

instance Core.AWSRequest CreateScheduleGroup where
  type
    AWSResponse CreateScheduleGroup =
      CreateScheduleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateScheduleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ScheduleGroupArn")
      )

instance Prelude.Hashable CreateScheduleGroup where
  hashWithSalt _salt CreateScheduleGroup' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateScheduleGroup where
  rnf CreateScheduleGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateScheduleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateScheduleGroup where
  toJSON CreateScheduleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateScheduleGroup where
  toPath CreateScheduleGroup' {..} =
    Prelude.mconcat
      ["/schedule-groups/", Data.toBS name]

instance Data.ToQuery CreateScheduleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateScheduleGroupResponse' smart constructor.
data CreateScheduleGroupResponse = CreateScheduleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the schedule group.
    scheduleGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScheduleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createScheduleGroupResponse_httpStatus' - The response's http status code.
--
-- 'scheduleGroupArn', 'createScheduleGroupResponse_scheduleGroupArn' - The Amazon Resource Name (ARN) of the schedule group.
newCreateScheduleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scheduleGroupArn'
  Prelude.Text ->
  CreateScheduleGroupResponse
newCreateScheduleGroupResponse
  pHttpStatus_
  pScheduleGroupArn_ =
    CreateScheduleGroupResponse'
      { httpStatus =
          pHttpStatus_,
        scheduleGroupArn = pScheduleGroupArn_
      }

-- | The response's http status code.
createScheduleGroupResponse_httpStatus :: Lens.Lens' CreateScheduleGroupResponse Prelude.Int
createScheduleGroupResponse_httpStatus = Lens.lens (\CreateScheduleGroupResponse' {httpStatus} -> httpStatus) (\s@CreateScheduleGroupResponse' {} a -> s {httpStatus = a} :: CreateScheduleGroupResponse)

-- | The Amazon Resource Name (ARN) of the schedule group.
createScheduleGroupResponse_scheduleGroupArn :: Lens.Lens' CreateScheduleGroupResponse Prelude.Text
createScheduleGroupResponse_scheduleGroupArn = Lens.lens (\CreateScheduleGroupResponse' {scheduleGroupArn} -> scheduleGroupArn) (\s@CreateScheduleGroupResponse' {} a -> s {scheduleGroupArn = a} :: CreateScheduleGroupResponse)

instance Prelude.NFData CreateScheduleGroupResponse where
  rnf CreateScheduleGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf scheduleGroupArn
