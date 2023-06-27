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
-- Module      : Amazonka.StepFunctions.CreateActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an activity. An activity is a task that you write in any
-- programming language and host on any machine that has access to Step
-- Functions. Activities must poll Step Functions using the
-- @GetActivityTask@ API action and respond using @SendTask*@ API actions.
-- This function lets Step Functions know the existence of your activity
-- and returns an identifier for use in a state machine and when polling
-- from the activity.
--
-- This operation is eventually consistent. The results are best effort and
-- may not reflect very recent updates and changes.
--
-- @CreateActivity@ is an idempotent API. Subsequent requests won’t create
-- a duplicate resource if it was already created. @CreateActivity@\'s
-- idempotency check is based on the activity @name@. If a following
-- request has different @tags@ values, Step Functions will ignore these
-- differences and treat it as an idempotent request of the previous. In
-- this case, @tags@ will not be updated, even if they are different.
module Amazonka.StepFunctions.CreateActivity
  ( -- * Creating a Request
    CreateActivity (..),
    newCreateActivity,

    -- * Request Lenses
    createActivity_tags,
    createActivity_name,

    -- * Destructuring the Response
    CreateActivityResponse (..),
    newCreateActivityResponse,

    -- * Response Lenses
    createActivityResponse_httpStatus,
    createActivityResponse_activityArn,
    createActivityResponse_creationDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newCreateActivity' smart constructor.
data CreateActivity = CreateActivity'
  { -- | The list of tags to add to a resource.
    --
    -- An array of key-value pairs. For more information, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
    -- in the /Amazon Web Services Billing and Cost Management User Guide/, and
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
    --
    -- Tags may only contain Unicode letters, digits, white space, or these
    -- symbols: @_ . : \/ = + - \@@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the activity to create. This name must be unique for your
    -- Amazon Web Services account and region for 90 days. For more
    -- information, see
    -- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
    -- in the /Step Functions Developer Guide/.
    --
    -- A name must /not/ contain:
    --
    -- -   white space
    --
    -- -   brackets @\< > { } [ ]@
    --
    -- -   wildcard characters @? *@
    --
    -- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
    --
    -- -   control characters (@U+0000-001F@, @U+007F-009F@)
    --
    -- To enable logging with CloudWatch Logs, the name should only contain
    -- 0-9, A-Z, a-z, - and _.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createActivity_tags' - The list of tags to add to a resource.
--
-- An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/, and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- Tags may only contain Unicode letters, digits, white space, or these
-- symbols: @_ . : \/ = + - \@@.
--
-- 'name', 'createActivity_name' - The name of the activity to create. This name must be unique for your
-- Amazon Web Services account and region for 90 days. For more
-- information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /Step Functions Developer Guide/.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
newCreateActivity ::
  -- | 'name'
  Prelude.Text ->
  CreateActivity
newCreateActivity pName_ =
  CreateActivity'
    { tags = Prelude.Nothing,
      name = pName_
    }

-- | The list of tags to add to a resource.
--
-- An array of key-value pairs. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /Amazon Web Services Billing and Cost Management User Guide/, and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags>.
--
-- Tags may only contain Unicode letters, digits, white space, or these
-- symbols: @_ . : \/ = + - \@@.
createActivity_tags :: Lens.Lens' CreateActivity (Prelude.Maybe [Tag])
createActivity_tags = Lens.lens (\CreateActivity' {tags} -> tags) (\s@CreateActivity' {} a -> s {tags = a} :: CreateActivity) Prelude.. Lens.mapping Lens.coerced

-- | The name of the activity to create. This name must be unique for your
-- Amazon Web Services account and region for 90 days. For more
-- information, see
-- <https://docs.aws.amazon.com/step-functions/latest/dg/limits.html#service-limits-state-machine-executions Limits Related to State Machine Executions>
-- in the /Step Functions Developer Guide/.
--
-- A name must /not/ contain:
--
-- -   white space
--
-- -   brackets @\< > { } [ ]@
--
-- -   wildcard characters @? *@
--
-- -   special characters @\" # % \\ ^ | ~ \` $ & , ; : \/@
--
-- -   control characters (@U+0000-001F@, @U+007F-009F@)
--
-- To enable logging with CloudWatch Logs, the name should only contain
-- 0-9, A-Z, a-z, - and _.
createActivity_name :: Lens.Lens' CreateActivity Prelude.Text
createActivity_name = Lens.lens (\CreateActivity' {name} -> name) (\s@CreateActivity' {} a -> s {name = a} :: CreateActivity)

instance Core.AWSRequest CreateActivity where
  type
    AWSResponse CreateActivity =
      CreateActivityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateActivityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "activityArn")
            Prelude.<*> (x Data..:> "creationDate")
      )

instance Prelude.Hashable CreateActivity where
  hashWithSalt _salt CreateActivity' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateActivity where
  rnf CreateActivity' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateActivity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.CreateActivity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateActivity where
  toJSON CreateActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateActivity where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateActivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateActivityResponse' smart constructor.
data CreateActivityResponse = CreateActivityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) that identifies the created activity.
    activityArn :: Prelude.Text,
    -- | The date the activity is created.
    creationDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateActivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createActivityResponse_httpStatus' - The response's http status code.
--
-- 'activityArn', 'createActivityResponse_activityArn' - The Amazon Resource Name (ARN) that identifies the created activity.
--
-- 'creationDate', 'createActivityResponse_creationDate' - The date the activity is created.
newCreateActivityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'activityArn'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  CreateActivityResponse
newCreateActivityResponse
  pHttpStatus_
  pActivityArn_
  pCreationDate_ =
    CreateActivityResponse'
      { httpStatus = pHttpStatus_,
        activityArn = pActivityArn_,
        creationDate = Data._Time Lens.# pCreationDate_
      }

-- | The response's http status code.
createActivityResponse_httpStatus :: Lens.Lens' CreateActivityResponse Prelude.Int
createActivityResponse_httpStatus = Lens.lens (\CreateActivityResponse' {httpStatus} -> httpStatus) (\s@CreateActivityResponse' {} a -> s {httpStatus = a} :: CreateActivityResponse)

-- | The Amazon Resource Name (ARN) that identifies the created activity.
createActivityResponse_activityArn :: Lens.Lens' CreateActivityResponse Prelude.Text
createActivityResponse_activityArn = Lens.lens (\CreateActivityResponse' {activityArn} -> activityArn) (\s@CreateActivityResponse' {} a -> s {activityArn = a} :: CreateActivityResponse)

-- | The date the activity is created.
createActivityResponse_creationDate :: Lens.Lens' CreateActivityResponse Prelude.UTCTime
createActivityResponse_creationDate = Lens.lens (\CreateActivityResponse' {creationDate} -> creationDate) (\s@CreateActivityResponse' {} a -> s {creationDate = a} :: CreateActivityResponse) Prelude.. Data._Time

instance Prelude.NFData CreateActivityResponse where
  rnf CreateActivityResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf activityArn
      `Prelude.seq` Prelude.rnf creationDate
