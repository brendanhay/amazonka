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
-- Module      : Amazonka.ResilienceHub.DescribeDraftAppVersionResourcesImportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of importing resources to an application version.
module Amazonka.ResilienceHub.DescribeDraftAppVersionResourcesImportStatus
  ( -- * Creating a Request
    DescribeDraftAppVersionResourcesImportStatus (..),
    newDescribeDraftAppVersionResourcesImportStatus,

    -- * Request Lenses
    describeDraftAppVersionResourcesImportStatus_appArn,

    -- * Destructuring the Response
    DescribeDraftAppVersionResourcesImportStatusResponse (..),
    newDescribeDraftAppVersionResourcesImportStatusResponse,

    -- * Response Lenses
    describeDraftAppVersionResourcesImportStatusResponse_errorMessage,
    describeDraftAppVersionResourcesImportStatusResponse_httpStatus,
    describeDraftAppVersionResourcesImportStatusResponse_appArn,
    describeDraftAppVersionResourcesImportStatusResponse_appVersion,
    describeDraftAppVersionResourcesImportStatusResponse_status,
    describeDraftAppVersionResourcesImportStatusResponse_statusChangeTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDraftAppVersionResourcesImportStatus' smart constructor.
data DescribeDraftAppVersionResourcesImportStatus = DescribeDraftAppVersionResourcesImportStatus'
  { -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDraftAppVersionResourcesImportStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'describeDraftAppVersionResourcesImportStatus_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newDescribeDraftAppVersionResourcesImportStatus ::
  -- | 'appArn'
  Prelude.Text ->
  DescribeDraftAppVersionResourcesImportStatus
newDescribeDraftAppVersionResourcesImportStatus
  pAppArn_ =
    DescribeDraftAppVersionResourcesImportStatus'
      { appArn =
          pAppArn_
      }

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeDraftAppVersionResourcesImportStatus_appArn :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatus Prelude.Text
describeDraftAppVersionResourcesImportStatus_appArn = Lens.lens (\DescribeDraftAppVersionResourcesImportStatus' {appArn} -> appArn) (\s@DescribeDraftAppVersionResourcesImportStatus' {} a -> s {appArn = a} :: DescribeDraftAppVersionResourcesImportStatus)

instance
  Core.AWSRequest
    DescribeDraftAppVersionResourcesImportStatus
  where
  type
    AWSResponse
      DescribeDraftAppVersionResourcesImportStatus =
      DescribeDraftAppVersionResourcesImportStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDraftAppVersionResourcesImportStatusResponse'
            Prelude.<$> (x Data..?> "errorMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "statusChangeTime")
      )

instance
  Prelude.Hashable
    DescribeDraftAppVersionResourcesImportStatus
  where
  hashWithSalt
    _salt
    DescribeDraftAppVersionResourcesImportStatus' {..} =
      _salt `Prelude.hashWithSalt` appArn

instance
  Prelude.NFData
    DescribeDraftAppVersionResourcesImportStatus
  where
  rnf DescribeDraftAppVersionResourcesImportStatus' {..} =
    Prelude.rnf appArn

instance
  Data.ToHeaders
    DescribeDraftAppVersionResourcesImportStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeDraftAppVersionResourcesImportStatus
  where
  toJSON
    DescribeDraftAppVersionResourcesImportStatus' {..} =
      Data.object
        ( Prelude.catMaybes
            [Prelude.Just ("appArn" Data..= appArn)]
        )

instance
  Data.ToPath
    DescribeDraftAppVersionResourcesImportStatus
  where
  toPath =
    Prelude.const
      "/describe-draft-app-version-resources-import-status"

instance
  Data.ToQuery
    DescribeDraftAppVersionResourcesImportStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDraftAppVersionResourcesImportStatusResponse' smart constructor.
data DescribeDraftAppVersionResourcesImportStatusResponse = DescribeDraftAppVersionResourcesImportStatusResponse'
  { -- | The returned error message for the request.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text,
    -- | The status of the action.
    status :: ResourceImportStatusType,
    -- | The timestamp for when the status last changed.
    statusChangeTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDraftAppVersionResourcesImportStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'describeDraftAppVersionResourcesImportStatusResponse_errorMessage' - The returned error message for the request.
--
-- 'httpStatus', 'describeDraftAppVersionResourcesImportStatusResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeDraftAppVersionResourcesImportStatusResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'describeDraftAppVersionResourcesImportStatusResponse_appVersion' - The version of the application.
--
-- 'status', 'describeDraftAppVersionResourcesImportStatusResponse_status' - The status of the action.
--
-- 'statusChangeTime', 'describeDraftAppVersionResourcesImportStatusResponse_statusChangeTime' - The timestamp for when the status last changed.
newDescribeDraftAppVersionResourcesImportStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'status'
  ResourceImportStatusType ->
  -- | 'statusChangeTime'
  Prelude.UTCTime ->
  DescribeDraftAppVersionResourcesImportStatusResponse
newDescribeDraftAppVersionResourcesImportStatusResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_
  pStatus_
  pStatusChangeTime_ =
    DescribeDraftAppVersionResourcesImportStatusResponse'
      { errorMessage =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_,
        appArn = pAppArn_,
        appVersion =
          pAppVersion_,
        status = pStatus_,
        statusChangeTime =
          Data._Time
            Lens.# pStatusChangeTime_
      }

-- | The returned error message for the request.
describeDraftAppVersionResourcesImportStatusResponse_errorMessage :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse (Prelude.Maybe Prelude.Text)
describeDraftAppVersionResourcesImportStatusResponse_errorMessage = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {errorMessage} -> errorMessage) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {errorMessage = a} :: DescribeDraftAppVersionResourcesImportStatusResponse)

-- | The response's http status code.
describeDraftAppVersionResourcesImportStatusResponse_httpStatus :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse Prelude.Int
describeDraftAppVersionResourcesImportStatusResponse_httpStatus = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {httpStatus = a} :: DescribeDraftAppVersionResourcesImportStatusResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeDraftAppVersionResourcesImportStatusResponse_appArn :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse Prelude.Text
describeDraftAppVersionResourcesImportStatusResponse_appArn = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {appArn} -> appArn) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {appArn = a} :: DescribeDraftAppVersionResourcesImportStatusResponse)

-- | The version of the application.
describeDraftAppVersionResourcesImportStatusResponse_appVersion :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse Prelude.Text
describeDraftAppVersionResourcesImportStatusResponse_appVersion = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {appVersion} -> appVersion) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {appVersion = a} :: DescribeDraftAppVersionResourcesImportStatusResponse)

-- | The status of the action.
describeDraftAppVersionResourcesImportStatusResponse_status :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse ResourceImportStatusType
describeDraftAppVersionResourcesImportStatusResponse_status = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {status} -> status) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {status = a} :: DescribeDraftAppVersionResourcesImportStatusResponse)

-- | The timestamp for when the status last changed.
describeDraftAppVersionResourcesImportStatusResponse_statusChangeTime :: Lens.Lens' DescribeDraftAppVersionResourcesImportStatusResponse Prelude.UTCTime
describeDraftAppVersionResourcesImportStatusResponse_statusChangeTime = Lens.lens (\DescribeDraftAppVersionResourcesImportStatusResponse' {statusChangeTime} -> statusChangeTime) (\s@DescribeDraftAppVersionResourcesImportStatusResponse' {} a -> s {statusChangeTime = a} :: DescribeDraftAppVersionResourcesImportStatusResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    DescribeDraftAppVersionResourcesImportStatusResponse
  where
  rnf
    DescribeDraftAppVersionResourcesImportStatusResponse' {..} =
      Prelude.rnf errorMessage
        `Prelude.seq` Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf appArn
        `Prelude.seq` Prelude.rnf appVersion
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf statusChangeTime
