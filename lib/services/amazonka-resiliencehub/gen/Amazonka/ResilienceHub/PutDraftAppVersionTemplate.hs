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
-- Module      : Amazonka.ResilienceHub.PutDraftAppVersionTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the app template for a draft version of a Resilience Hub
-- app.
module Amazonka.ResilienceHub.PutDraftAppVersionTemplate
  ( -- * Creating a Request
    PutDraftAppVersionTemplate (..),
    newPutDraftAppVersionTemplate,

    -- * Request Lenses
    putDraftAppVersionTemplate_appArn,
    putDraftAppVersionTemplate_appTemplateBody,

    -- * Destructuring the Response
    PutDraftAppVersionTemplateResponse (..),
    newPutDraftAppVersionTemplateResponse,

    -- * Response Lenses
    putDraftAppVersionTemplateResponse_appVersion,
    putDraftAppVersionTemplateResponse_appArn,
    putDraftAppVersionTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDraftAppVersionTemplate' smart constructor.
data PutDraftAppVersionTemplate = PutDraftAppVersionTemplate'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | A JSON string that contains the body of the app template.
    appTemplateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDraftAppVersionTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'putDraftAppVersionTemplate_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appTemplateBody', 'putDraftAppVersionTemplate_appTemplateBody' - A JSON string that contains the body of the app template.
newPutDraftAppVersionTemplate ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appTemplateBody'
  Prelude.Text ->
  PutDraftAppVersionTemplate
newPutDraftAppVersionTemplate
  pAppArn_
  pAppTemplateBody_ =
    PutDraftAppVersionTemplate'
      { appArn = pAppArn_,
        appTemplateBody = pAppTemplateBody_
      }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
putDraftAppVersionTemplate_appArn :: Lens.Lens' PutDraftAppVersionTemplate Prelude.Text
putDraftAppVersionTemplate_appArn = Lens.lens (\PutDraftAppVersionTemplate' {appArn} -> appArn) (\s@PutDraftAppVersionTemplate' {} a -> s {appArn = a} :: PutDraftAppVersionTemplate)

-- | A JSON string that contains the body of the app template.
putDraftAppVersionTemplate_appTemplateBody :: Lens.Lens' PutDraftAppVersionTemplate Prelude.Text
putDraftAppVersionTemplate_appTemplateBody = Lens.lens (\PutDraftAppVersionTemplate' {appTemplateBody} -> appTemplateBody) (\s@PutDraftAppVersionTemplate' {} a -> s {appTemplateBody = a} :: PutDraftAppVersionTemplate)

instance Core.AWSRequest PutDraftAppVersionTemplate where
  type
    AWSResponse PutDraftAppVersionTemplate =
      PutDraftAppVersionTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDraftAppVersionTemplateResponse'
            Prelude.<$> (x Data..?> "appVersion")
            Prelude.<*> (x Data..?> "appArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDraftAppVersionTemplate where
  hashWithSalt _salt PutDraftAppVersionTemplate' {..} =
    _salt `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appTemplateBody

instance Prelude.NFData PutDraftAppVersionTemplate where
  rnf PutDraftAppVersionTemplate' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appTemplateBody

instance Data.ToHeaders PutDraftAppVersionTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDraftAppVersionTemplate where
  toJSON PutDraftAppVersionTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just
              ("appTemplateBody" Data..= appTemplateBody)
          ]
      )

instance Data.ToPath PutDraftAppVersionTemplate where
  toPath =
    Prelude.const "/put-draft-app-version-template"

instance Data.ToQuery PutDraftAppVersionTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDraftAppVersionTemplateResponse' smart constructor.
data PutDraftAppVersionTemplateResponse = PutDraftAppVersionTemplateResponse'
  { -- | The version of the application.
    appVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDraftAppVersionTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appVersion', 'putDraftAppVersionTemplateResponse_appVersion' - The version of the application.
--
-- 'appArn', 'putDraftAppVersionTemplateResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'httpStatus', 'putDraftAppVersionTemplateResponse_httpStatus' - The response's http status code.
newPutDraftAppVersionTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDraftAppVersionTemplateResponse
newPutDraftAppVersionTemplateResponse pHttpStatus_ =
  PutDraftAppVersionTemplateResponse'
    { appVersion =
        Prelude.Nothing,
      appArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the application.
putDraftAppVersionTemplateResponse_appVersion :: Lens.Lens' PutDraftAppVersionTemplateResponse (Prelude.Maybe Prelude.Text)
putDraftAppVersionTemplateResponse_appVersion = Lens.lens (\PutDraftAppVersionTemplateResponse' {appVersion} -> appVersion) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {appVersion = a} :: PutDraftAppVersionTemplateResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
putDraftAppVersionTemplateResponse_appArn :: Lens.Lens' PutDraftAppVersionTemplateResponse (Prelude.Maybe Prelude.Text)
putDraftAppVersionTemplateResponse_appArn = Lens.lens (\PutDraftAppVersionTemplateResponse' {appArn} -> appArn) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {appArn = a} :: PutDraftAppVersionTemplateResponse)

-- | The response's http status code.
putDraftAppVersionTemplateResponse_httpStatus :: Lens.Lens' PutDraftAppVersionTemplateResponse Prelude.Int
putDraftAppVersionTemplateResponse_httpStatus = Lens.lens (\PutDraftAppVersionTemplateResponse' {httpStatus} -> httpStatus) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {httpStatus = a} :: PutDraftAppVersionTemplateResponse)

instance
  Prelude.NFData
    PutDraftAppVersionTemplateResponse
  where
  rnf PutDraftAppVersionTemplateResponse' {..} =
    Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf httpStatus
