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
-- Module      : Amazonka.ResilienceHub.CreateApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Resilience Hub application. A Resilience Hub application is a
-- collection of Amazon Web Services resources structured to prevent and
-- recover Amazon Web Services application disruptions. To describe a
-- Resilience Hub application, you provide an application name, resources
-- from one or more–up to five–CloudFormation stacks, and an appropriate
-- resiliency policy.
--
-- After you create a Resilience Hub application, you publish it so that
-- you can run a resiliency assessment on it. You can then use
-- recommendations from the assessment to improve resiliency by running
-- another assessment, comparing results, and then iterating the process
-- until you achieve your goals for recovery time objective (RTO) and
-- recovery point objective (RPO).
module Amazonka.ResilienceHub.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_assessmentSchedule,
    createApp_clientToken,
    createApp_description,
    createApp_policyArn,
    createApp_tags,
    createApp_name,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_httpStatus,
    createAppResponse_app,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
    assessmentSchedule :: Prelude.Maybe AppAssessmentScheduleType,
    -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The name for the application.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentSchedule', 'createApp_assessmentSchedule' - Assessment execution schedule with \'Daily\' or \'Disabled\' values.
--
-- 'clientToken', 'createApp_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'description', 'createApp_description' - The optional description for an app.
--
-- 'policyArn', 'createApp_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'tags', 'createApp_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'name', 'createApp_name' - The name for the application.
newCreateApp ::
  -- | 'name'
  Prelude.Text ->
  CreateApp
newCreateApp pName_ =
  CreateApp'
    { assessmentSchedule = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
createApp_assessmentSchedule :: Lens.Lens' CreateApp (Prelude.Maybe AppAssessmentScheduleType)
createApp_assessmentSchedule = Lens.lens (\CreateApp' {assessmentSchedule} -> assessmentSchedule) (\s@CreateApp' {} a -> s {assessmentSchedule = a} :: CreateApp)

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
createApp_clientToken :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_clientToken = Lens.lens (\CreateApp' {clientToken} -> clientToken) (\s@CreateApp' {} a -> s {clientToken = a} :: CreateApp)

-- | The optional description for an app.
createApp_description :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
createApp_policyArn :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_policyArn = Lens.lens (\CreateApp' {policyArn} -> policyArn) (\s@CreateApp' {} a -> s {policyArn = a} :: CreateApp)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
createApp_tags :: Lens.Lens' CreateApp (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApp_tags = Lens.lens (\CreateApp' {tags} -> tags) (\s@CreateApp' {} a -> s {tags = a} :: CreateApp) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name for the application.
createApp_name :: Lens.Lens' CreateApp Prelude.Text
createApp_name = Lens.lens (\CreateApp' {name} -> name) (\s@CreateApp' {} a -> s {name = a} :: CreateApp)

instance Core.AWSRequest CreateApp where
  type AWSResponse CreateApp = CreateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "app")
      )

instance Prelude.Hashable CreateApp where
  hashWithSalt _salt CreateApp' {..} =
    _salt `Prelude.hashWithSalt` assessmentSchedule
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateApp where
  rnf CreateApp' {..} =
    Prelude.rnf assessmentSchedule
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assessmentSchedule" Data..=)
              Prelude.<$> assessmentSchedule,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("policyArn" Data..=) Prelude.<$> policyArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateApp where
  toPath = Prelude.const "/create-app"

instance Data.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The created application returned as an object with details including
    -- compliance status, creation time, description, resiliency score, and
    -- more.
    app :: App
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
--
-- 'app', 'createAppResponse_app' - The created application returned as an object with details including
-- compliance status, creation time, description, resiliency score, and
-- more.
newCreateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'app'
  App ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ pApp_ =
  CreateAppResponse'
    { httpStatus = pHttpStatus_,
      app = pApp_
    }

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

-- | The created application returned as an object with details including
-- compliance status, creation time, description, resiliency score, and
-- more.
createAppResponse_app :: Lens.Lens' CreateAppResponse App
createAppResponse_app = Lens.lens (\CreateAppResponse' {app} -> app) (\s@CreateAppResponse' {} a -> s {app = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse where
  rnf CreateAppResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf app
