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
-- Module      : Amazonka.ResilienceHub.DescribeAppVersionTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about an AWS Resilience Hub
module Amazonka.ResilienceHub.DescribeAppVersionTemplate
  ( -- * Creating a Request
    DescribeAppVersionTemplate (..),
    newDescribeAppVersionTemplate,

    -- * Request Lenses
    describeAppVersionTemplate_appArn,
    describeAppVersionTemplate_appVersion,

    -- * Destructuring the Response
    DescribeAppVersionTemplateResponse (..),
    newDescribeAppVersionTemplateResponse,

    -- * Response Lenses
    describeAppVersionTemplateResponse_httpStatus,
    describeAppVersionTemplateResponse_appArn,
    describeAppVersionTemplateResponse_appTemplateBody,
    describeAppVersionTemplateResponse_appVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppVersionTemplate' smart constructor.
data DescribeAppVersionTemplate = DescribeAppVersionTemplate'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppVersionTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'describeAppVersionTemplate_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'describeAppVersionTemplate_appVersion' - The version of the application.
newDescribeAppVersionTemplate ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionTemplate
newDescribeAppVersionTemplate pAppArn_ pAppVersion_ =
  DescribeAppVersionTemplate'
    { appArn = pAppArn_,
      appVersion = pAppVersion_
    }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAppVersionTemplate_appArn :: Lens.Lens' DescribeAppVersionTemplate Prelude.Text
describeAppVersionTemplate_appArn = Lens.lens (\DescribeAppVersionTemplate' {appArn} -> appArn) (\s@DescribeAppVersionTemplate' {} a -> s {appArn = a} :: DescribeAppVersionTemplate)

-- | The version of the application.
describeAppVersionTemplate_appVersion :: Lens.Lens' DescribeAppVersionTemplate Prelude.Text
describeAppVersionTemplate_appVersion = Lens.lens (\DescribeAppVersionTemplate' {appVersion} -> appVersion) (\s@DescribeAppVersionTemplate' {} a -> s {appVersion = a} :: DescribeAppVersionTemplate)

instance Core.AWSRequest DescribeAppVersionTemplate where
  type
    AWSResponse DescribeAppVersionTemplate =
      DescribeAppVersionTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppVersionTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appTemplateBody")
            Prelude.<*> (x Data..:> "appVersion")
      )

instance Prelude.Hashable DescribeAppVersionTemplate where
  hashWithSalt _salt DescribeAppVersionTemplate' {..} =
    _salt `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appVersion

instance Prelude.NFData DescribeAppVersionTemplate where
  rnf DescribeAppVersionTemplate' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance Data.ToHeaders DescribeAppVersionTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppVersionTemplate where
  toJSON DescribeAppVersionTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath DescribeAppVersionTemplate where
  toPath =
    Prelude.const "/describe-app-version-template"

instance Data.ToQuery DescribeAppVersionTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppVersionTemplateResponse' smart constructor.
data DescribeAppVersionTemplateResponse = DescribeAppVersionTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The body of the template.
    appTemplateBody :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppVersionTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAppVersionTemplateResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeAppVersionTemplateResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appTemplateBody', 'describeAppVersionTemplateResponse_appTemplateBody' - The body of the template.
--
-- 'appVersion', 'describeAppVersionTemplateResponse_appVersion' - The version of the application.
newDescribeAppVersionTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appTemplateBody'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionTemplateResponse
newDescribeAppVersionTemplateResponse
  pHttpStatus_
  pAppArn_
  pAppTemplateBody_
  pAppVersion_ =
    DescribeAppVersionTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        appArn = pAppArn_,
        appTemplateBody = pAppTemplateBody_,
        appVersion = pAppVersion_
      }

-- | The response's http status code.
describeAppVersionTemplateResponse_httpStatus :: Lens.Lens' DescribeAppVersionTemplateResponse Prelude.Int
describeAppVersionTemplateResponse_httpStatus = Lens.lens (\DescribeAppVersionTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeAppVersionTemplateResponse' {} a -> s {httpStatus = a} :: DescribeAppVersionTemplateResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAppVersionTemplateResponse_appArn :: Lens.Lens' DescribeAppVersionTemplateResponse Prelude.Text
describeAppVersionTemplateResponse_appArn = Lens.lens (\DescribeAppVersionTemplateResponse' {appArn} -> appArn) (\s@DescribeAppVersionTemplateResponse' {} a -> s {appArn = a} :: DescribeAppVersionTemplateResponse)

-- | The body of the template.
describeAppVersionTemplateResponse_appTemplateBody :: Lens.Lens' DescribeAppVersionTemplateResponse Prelude.Text
describeAppVersionTemplateResponse_appTemplateBody = Lens.lens (\DescribeAppVersionTemplateResponse' {appTemplateBody} -> appTemplateBody) (\s@DescribeAppVersionTemplateResponse' {} a -> s {appTemplateBody = a} :: DescribeAppVersionTemplateResponse)

-- | The version of the application.
describeAppVersionTemplateResponse_appVersion :: Lens.Lens' DescribeAppVersionTemplateResponse Prelude.Text
describeAppVersionTemplateResponse_appVersion = Lens.lens (\DescribeAppVersionTemplateResponse' {appVersion} -> appVersion) (\s@DescribeAppVersionTemplateResponse' {} a -> s {appVersion = a} :: DescribeAppVersionTemplateResponse)

instance
  Prelude.NFData
    DescribeAppVersionTemplateResponse
  where
  rnf DescribeAppVersionTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appTemplateBody
      `Prelude.seq` Prelude.rnf appVersion
