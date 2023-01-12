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
-- Module      : Amazonka.ResilienceHub.StartAppAssessment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new application assessment for an application.
module Amazonka.ResilienceHub.StartAppAssessment
  ( -- * Creating a Request
    StartAppAssessment (..),
    newStartAppAssessment,

    -- * Request Lenses
    startAppAssessment_clientToken,
    startAppAssessment_tags,
    startAppAssessment_appArn,
    startAppAssessment_appVersion,
    startAppAssessment_assessmentName,

    -- * Destructuring the Response
    StartAppAssessmentResponse (..),
    newStartAppAssessmentResponse,

    -- * Response Lenses
    startAppAssessmentResponse_httpStatus,
    startAppAssessmentResponse_assessment,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAppAssessment' smart constructor.
data StartAppAssessment = StartAppAssessment'
  { -- | Used for an idempotency token. A client token is a unique,
    -- case-sensitive string of up to 64 ASCII characters. You should not reuse
    -- the same client token for other API requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the resource. A tag is a label that you assign to
    -- an Amazon Web Services resource. Each tag consists of a key\/value pair.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text,
    -- | The name for the assessment.
    assessmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAppAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startAppAssessment_clientToken' - Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
--
-- 'tags', 'startAppAssessment_tags' - The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
--
-- 'appArn', 'startAppAssessment_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'startAppAssessment_appVersion' - The version of the application.
--
-- 'assessmentName', 'startAppAssessment_assessmentName' - The name for the assessment.
newStartAppAssessment ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'assessmentName'
  Prelude.Text ->
  StartAppAssessment
newStartAppAssessment
  pAppArn_
  pAppVersion_
  pAssessmentName_ =
    StartAppAssessment'
      { clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        appArn = pAppArn_,
        appVersion = pAppVersion_,
        assessmentName = pAssessmentName_
      }

-- | Used for an idempotency token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. You should not reuse
-- the same client token for other API requests.
startAppAssessment_clientToken :: Lens.Lens' StartAppAssessment (Prelude.Maybe Prelude.Text)
startAppAssessment_clientToken = Lens.lens (\StartAppAssessment' {clientToken} -> clientToken) (\s@StartAppAssessment' {} a -> s {clientToken = a} :: StartAppAssessment)

-- | The tags assigned to the resource. A tag is a label that you assign to
-- an Amazon Web Services resource. Each tag consists of a key\/value pair.
startAppAssessment_tags :: Lens.Lens' StartAppAssessment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startAppAssessment_tags = Lens.lens (\StartAppAssessment' {tags} -> tags) (\s@StartAppAssessment' {} a -> s {tags = a} :: StartAppAssessment) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
startAppAssessment_appArn :: Lens.Lens' StartAppAssessment Prelude.Text
startAppAssessment_appArn = Lens.lens (\StartAppAssessment' {appArn} -> appArn) (\s@StartAppAssessment' {} a -> s {appArn = a} :: StartAppAssessment)

-- | The version of the application.
startAppAssessment_appVersion :: Lens.Lens' StartAppAssessment Prelude.Text
startAppAssessment_appVersion = Lens.lens (\StartAppAssessment' {appVersion} -> appVersion) (\s@StartAppAssessment' {} a -> s {appVersion = a} :: StartAppAssessment)

-- | The name for the assessment.
startAppAssessment_assessmentName :: Lens.Lens' StartAppAssessment Prelude.Text
startAppAssessment_assessmentName = Lens.lens (\StartAppAssessment' {assessmentName} -> assessmentName) (\s@StartAppAssessment' {} a -> s {assessmentName = a} :: StartAppAssessment)

instance Core.AWSRequest StartAppAssessment where
  type
    AWSResponse StartAppAssessment =
      StartAppAssessmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAppAssessmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assessment")
      )

instance Prelude.Hashable StartAppAssessment where
  hashWithSalt _salt StartAppAssessment' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion
      `Prelude.hashWithSalt` assessmentName

instance Prelude.NFData StartAppAssessment where
  rnf StartAppAssessment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf assessmentName

instance Data.ToHeaders StartAppAssessment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAppAssessment where
  toJSON StartAppAssessment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion),
            Prelude.Just
              ("assessmentName" Data..= assessmentName)
          ]
      )

instance Data.ToPath StartAppAssessment where
  toPath = Prelude.const "/start-app-assessment"

instance Data.ToQuery StartAppAssessment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAppAssessmentResponse' smart constructor.
data StartAppAssessmentResponse = StartAppAssessmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The assessment created.
    assessment :: AppAssessment
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAppAssessmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startAppAssessmentResponse_httpStatus' - The response's http status code.
--
-- 'assessment', 'startAppAssessmentResponse_assessment' - The assessment created.
newStartAppAssessmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assessment'
  AppAssessment ->
  StartAppAssessmentResponse
newStartAppAssessmentResponse
  pHttpStatus_
  pAssessment_ =
    StartAppAssessmentResponse'
      { httpStatus =
          pHttpStatus_,
        assessment = pAssessment_
      }

-- | The response's http status code.
startAppAssessmentResponse_httpStatus :: Lens.Lens' StartAppAssessmentResponse Prelude.Int
startAppAssessmentResponse_httpStatus = Lens.lens (\StartAppAssessmentResponse' {httpStatus} -> httpStatus) (\s@StartAppAssessmentResponse' {} a -> s {httpStatus = a} :: StartAppAssessmentResponse)

-- | The assessment created.
startAppAssessmentResponse_assessment :: Lens.Lens' StartAppAssessmentResponse AppAssessment
startAppAssessmentResponse_assessment = Lens.lens (\StartAppAssessmentResponse' {assessment} -> assessment) (\s@StartAppAssessmentResponse' {} a -> s {assessment = a} :: StartAppAssessmentResponse)

instance Prelude.NFData StartAppAssessmentResponse where
  rnf StartAppAssessmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assessment
