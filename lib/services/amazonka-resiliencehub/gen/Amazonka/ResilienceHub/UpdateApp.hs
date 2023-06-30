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
-- Module      : Amazonka.ResilienceHub.UpdateApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an application.
module Amazonka.ResilienceHub.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_assessmentSchedule,
    updateApp_clearResiliencyPolicyArn,
    updateApp_description,
    updateApp_policyArn,
    updateApp_appArn,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,

    -- * Response Lenses
    updateAppResponse_httpStatus,
    updateAppResponse_app,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
    assessmentSchedule :: Prelude.Maybe AppAssessmentScheduleType,
    -- | Specifies if the resiliency policy ARN should be cleared.
    clearResiliencyPolicyArn :: Prelude.Maybe Prelude.Bool,
    -- | The optional description for an app.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentSchedule', 'updateApp_assessmentSchedule' - Assessment execution schedule with \'Daily\' or \'Disabled\' values.
--
-- 'clearResiliencyPolicyArn', 'updateApp_clearResiliencyPolicyArn' - Specifies if the resiliency policy ARN should be cleared.
--
-- 'description', 'updateApp_description' - The optional description for an app.
--
-- 'policyArn', 'updateApp_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appArn', 'updateApp_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newUpdateApp ::
  -- | 'appArn'
  Prelude.Text ->
  UpdateApp
newUpdateApp pAppArn_ =
  UpdateApp'
    { assessmentSchedule = Prelude.Nothing,
      clearResiliencyPolicyArn = Prelude.Nothing,
      description = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      appArn = pAppArn_
    }

-- | Assessment execution schedule with \'Daily\' or \'Disabled\' values.
updateApp_assessmentSchedule :: Lens.Lens' UpdateApp (Prelude.Maybe AppAssessmentScheduleType)
updateApp_assessmentSchedule = Lens.lens (\UpdateApp' {assessmentSchedule} -> assessmentSchedule) (\s@UpdateApp' {} a -> s {assessmentSchedule = a} :: UpdateApp)

-- | Specifies if the resiliency policy ARN should be cleared.
updateApp_clearResiliencyPolicyArn :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Bool)
updateApp_clearResiliencyPolicyArn = Lens.lens (\UpdateApp' {clearResiliencyPolicyArn} -> clearResiliencyPolicyArn) (\s@UpdateApp' {} a -> s {clearResiliencyPolicyArn = a} :: UpdateApp)

-- | The optional description for an app.
updateApp_description :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateApp_policyArn :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_policyArn = Lens.lens (\UpdateApp' {policyArn} -> policyArn) (\s@UpdateApp' {} a -> s {policyArn = a} :: UpdateApp)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateApp_appArn :: Lens.Lens' UpdateApp Prelude.Text
updateApp_appArn = Lens.lens (\UpdateApp' {appArn} -> appArn) (\s@UpdateApp' {} a -> s {appArn = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "app")
      )

instance Prelude.Hashable UpdateApp where
  hashWithSalt _salt UpdateApp' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentSchedule
      `Prelude.hashWithSalt` clearResiliencyPolicyArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` appArn

instance Prelude.NFData UpdateApp where
  rnf UpdateApp' {..} =
    Prelude.rnf assessmentSchedule
      `Prelude.seq` Prelude.rnf clearResiliencyPolicyArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf appArn

instance Data.ToHeaders UpdateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assessmentSchedule" Data..=)
              Prelude.<$> assessmentSchedule,
            ("clearResiliencyPolicyArn" Data..=)
              Prelude.<$> clearResiliencyPolicyArn,
            ("description" Data..=) Prelude.<$> description,
            ("policyArn" Data..=) Prelude.<$> policyArn,
            Prelude.Just ("appArn" Data..= appArn)
          ]
      )

instance Data.ToPath UpdateApp where
  toPath = Prelude.const "/update-app"

instance Data.ToQuery UpdateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The specified application, returned as an object with details including
    -- compliance status, creation time, description, resiliency score, and
    -- more.
    app :: App
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAppResponse_httpStatus' - The response's http status code.
--
-- 'app', 'updateAppResponse_app' - The specified application, returned as an object with details including
-- compliance status, creation time, description, resiliency score, and
-- more.
newUpdateAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'app'
  App ->
  UpdateAppResponse
newUpdateAppResponse pHttpStatus_ pApp_ =
  UpdateAppResponse'
    { httpStatus = pHttpStatus_,
      app = pApp_
    }

-- | The response's http status code.
updateAppResponse_httpStatus :: Lens.Lens' UpdateAppResponse Prelude.Int
updateAppResponse_httpStatus = Lens.lens (\UpdateAppResponse' {httpStatus} -> httpStatus) (\s@UpdateAppResponse' {} a -> s {httpStatus = a} :: UpdateAppResponse)

-- | The specified application, returned as an object with details including
-- compliance status, creation time, description, resiliency score, and
-- more.
updateAppResponse_app :: Lens.Lens' UpdateAppResponse App
updateAppResponse_app = Lens.lens (\UpdateAppResponse' {app} -> app) (\s@UpdateAppResponse' {} a -> s {app = a} :: UpdateAppResponse)

instance Prelude.NFData UpdateAppResponse where
  rnf UpdateAppResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf app
