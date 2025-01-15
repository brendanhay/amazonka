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
-- Module      : Amazonka.ResilienceHub.PublishAppVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a new version of a specific Resilience Hub application.
module Amazonka.ResilienceHub.PublishAppVersion
  ( -- * Creating a Request
    PublishAppVersion (..),
    newPublishAppVersion,

    -- * Request Lenses
    publishAppVersion_appArn,

    -- * Destructuring the Response
    PublishAppVersionResponse (..),
    newPublishAppVersionResponse,

    -- * Response Lenses
    publishAppVersionResponse_appVersion,
    publishAppVersionResponse_httpStatus,
    publishAppVersionResponse_appArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishAppVersion' smart constructor.
data PublishAppVersion = PublishAppVersion'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishAppVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'publishAppVersion_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newPublishAppVersion ::
  -- | 'appArn'
  Prelude.Text ->
  PublishAppVersion
newPublishAppVersion pAppArn_ =
  PublishAppVersion' {appArn = pAppArn_}

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
publishAppVersion_appArn :: Lens.Lens' PublishAppVersion Prelude.Text
publishAppVersion_appArn = Lens.lens (\PublishAppVersion' {appArn} -> appArn) (\s@PublishAppVersion' {} a -> s {appArn = a} :: PublishAppVersion)

instance Core.AWSRequest PublishAppVersion where
  type
    AWSResponse PublishAppVersion =
      PublishAppVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishAppVersionResponse'
            Prelude.<$> (x Data..?> "appVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
      )

instance Prelude.Hashable PublishAppVersion where
  hashWithSalt _salt PublishAppVersion' {..} =
    _salt `Prelude.hashWithSalt` appArn

instance Prelude.NFData PublishAppVersion where
  rnf PublishAppVersion' {..} = Prelude.rnf appArn

instance Data.ToHeaders PublishAppVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PublishAppVersion where
  toJSON PublishAppVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("appArn" Data..= appArn)]
      )

instance Data.ToPath PublishAppVersion where
  toPath = Prelude.const "/publish-app-version"

instance Data.ToQuery PublishAppVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishAppVersionResponse' smart constructor.
data PublishAppVersionResponse = PublishAppVersionResponse'
  { -- | The version of the application.
    appVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishAppVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appVersion', 'publishAppVersionResponse_appVersion' - The version of the application.
--
-- 'httpStatus', 'publishAppVersionResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'publishAppVersionResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newPublishAppVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  PublishAppVersionResponse
newPublishAppVersionResponse pHttpStatus_ pAppArn_ =
  PublishAppVersionResponse'
    { appVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      appArn = pAppArn_
    }

-- | The version of the application.
publishAppVersionResponse_appVersion :: Lens.Lens' PublishAppVersionResponse (Prelude.Maybe Prelude.Text)
publishAppVersionResponse_appVersion = Lens.lens (\PublishAppVersionResponse' {appVersion} -> appVersion) (\s@PublishAppVersionResponse' {} a -> s {appVersion = a} :: PublishAppVersionResponse)

-- | The response's http status code.
publishAppVersionResponse_httpStatus :: Lens.Lens' PublishAppVersionResponse Prelude.Int
publishAppVersionResponse_httpStatus = Lens.lens (\PublishAppVersionResponse' {httpStatus} -> httpStatus) (\s@PublishAppVersionResponse' {} a -> s {httpStatus = a} :: PublishAppVersionResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
publishAppVersionResponse_appArn :: Lens.Lens' PublishAppVersionResponse Prelude.Text
publishAppVersionResponse_appArn = Lens.lens (\PublishAppVersionResponse' {appArn} -> appArn) (\s@PublishAppVersionResponse' {} a -> s {appArn = a} :: PublishAppVersionResponse)

instance Prelude.NFData PublishAppVersionResponse where
  rnf PublishAppVersionResponse' {..} =
    Prelude.rnf appVersion `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf appArn
