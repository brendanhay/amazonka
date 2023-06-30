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
-- Module      : Amazonka.ResilienceHub.DescribeAppVersionResourcesResolutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resolution status for the specified resolution identifier
-- for an application version. If @resolutionId@ is not specified, the
-- current resolution status is returned.
module Amazonka.ResilienceHub.DescribeAppVersionResourcesResolutionStatus
  ( -- * Creating a Request
    DescribeAppVersionResourcesResolutionStatus (..),
    newDescribeAppVersionResourcesResolutionStatus,

    -- * Request Lenses
    describeAppVersionResourcesResolutionStatus_resolutionId,
    describeAppVersionResourcesResolutionStatus_appArn,
    describeAppVersionResourcesResolutionStatus_appVersion,

    -- * Destructuring the Response
    DescribeAppVersionResourcesResolutionStatusResponse (..),
    newDescribeAppVersionResourcesResolutionStatusResponse,

    -- * Response Lenses
    describeAppVersionResourcesResolutionStatusResponse_errorMessage,
    describeAppVersionResourcesResolutionStatusResponse_httpStatus,
    describeAppVersionResourcesResolutionStatusResponse_appArn,
    describeAppVersionResourcesResolutionStatusResponse_appVersion,
    describeAppVersionResourcesResolutionStatusResponse_resolutionId,
    describeAppVersionResourcesResolutionStatusResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppVersionResourcesResolutionStatus' smart constructor.
data DescribeAppVersionResourcesResolutionStatus = DescribeAppVersionResourcesResolutionStatus'
  { -- | The identifier for a specific resolution.
    resolutionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
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
-- Create a value of 'DescribeAppVersionResourcesResolutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolutionId', 'describeAppVersionResourcesResolutionStatus_resolutionId' - The identifier for a specific resolution.
--
-- 'appArn', 'describeAppVersionResourcesResolutionStatus_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'describeAppVersionResourcesResolutionStatus_appVersion' - The version of the application.
newDescribeAppVersionResourcesResolutionStatus ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  DescribeAppVersionResourcesResolutionStatus
newDescribeAppVersionResourcesResolutionStatus
  pAppArn_
  pAppVersion_ =
    DescribeAppVersionResourcesResolutionStatus'
      { resolutionId =
          Prelude.Nothing,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The identifier for a specific resolution.
describeAppVersionResourcesResolutionStatus_resolutionId :: Lens.Lens' DescribeAppVersionResourcesResolutionStatus (Prelude.Maybe Prelude.Text)
describeAppVersionResourcesResolutionStatus_resolutionId = Lens.lens (\DescribeAppVersionResourcesResolutionStatus' {resolutionId} -> resolutionId) (\s@DescribeAppVersionResourcesResolutionStatus' {} a -> s {resolutionId = a} :: DescribeAppVersionResourcesResolutionStatus)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAppVersionResourcesResolutionStatus_appArn :: Lens.Lens' DescribeAppVersionResourcesResolutionStatus Prelude.Text
describeAppVersionResourcesResolutionStatus_appArn = Lens.lens (\DescribeAppVersionResourcesResolutionStatus' {appArn} -> appArn) (\s@DescribeAppVersionResourcesResolutionStatus' {} a -> s {appArn = a} :: DescribeAppVersionResourcesResolutionStatus)

-- | The version of the application.
describeAppVersionResourcesResolutionStatus_appVersion :: Lens.Lens' DescribeAppVersionResourcesResolutionStatus Prelude.Text
describeAppVersionResourcesResolutionStatus_appVersion = Lens.lens (\DescribeAppVersionResourcesResolutionStatus' {appVersion} -> appVersion) (\s@DescribeAppVersionResourcesResolutionStatus' {} a -> s {appVersion = a} :: DescribeAppVersionResourcesResolutionStatus)

instance
  Core.AWSRequest
    DescribeAppVersionResourcesResolutionStatus
  where
  type
    AWSResponse
      DescribeAppVersionResourcesResolutionStatus =
      DescribeAppVersionResourcesResolutionStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppVersionResourcesResolutionStatusResponse'
            Prelude.<$> (x Data..?> "errorMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "appArn")
            Prelude.<*> (x Data..:> "appVersion")
            Prelude.<*> (x Data..:> "resolutionId")
            Prelude.<*> (x Data..:> "status")
      )

instance
  Prelude.Hashable
    DescribeAppVersionResourcesResolutionStatus
  where
  hashWithSalt
    _salt
    DescribeAppVersionResourcesResolutionStatus' {..} =
      _salt
        `Prelude.hashWithSalt` resolutionId
        `Prelude.hashWithSalt` appArn
        `Prelude.hashWithSalt` appVersion

instance
  Prelude.NFData
    DescribeAppVersionResourcesResolutionStatus
  where
  rnf DescribeAppVersionResourcesResolutionStatus' {..} =
    Prelude.rnf resolutionId
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance
  Data.ToHeaders
    DescribeAppVersionResourcesResolutionStatus
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
    DescribeAppVersionResourcesResolutionStatus
  where
  toJSON
    DescribeAppVersionResourcesResolutionStatus' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("resolutionId" Data..=) Prelude.<$> resolutionId,
              Prelude.Just ("appArn" Data..= appArn),
              Prelude.Just ("appVersion" Data..= appVersion)
            ]
        )

instance
  Data.ToPath
    DescribeAppVersionResourcesResolutionStatus
  where
  toPath =
    Prelude.const
      "/describe-app-version-resources-resolution-status"

instance
  Data.ToQuery
    DescribeAppVersionResourcesResolutionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppVersionResourcesResolutionStatusResponse' smart constructor.
data DescribeAppVersionResourcesResolutionStatusResponse = DescribeAppVersionResourcesResolutionStatusResponse'
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
    -- | The identifier for a specific resolution.
    resolutionId :: Prelude.Text,
    -- | The status of the action.
    status :: ResourceResolutionStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppVersionResourcesResolutionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'describeAppVersionResourcesResolutionStatusResponse_errorMessage' - The returned error message for the request.
--
-- 'httpStatus', 'describeAppVersionResourcesResolutionStatusResponse_httpStatus' - The response's http status code.
--
-- 'appArn', 'describeAppVersionResourcesResolutionStatusResponse_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'describeAppVersionResourcesResolutionStatusResponse_appVersion' - The version of the application.
--
-- 'resolutionId', 'describeAppVersionResourcesResolutionStatusResponse_resolutionId' - The identifier for a specific resolution.
--
-- 'status', 'describeAppVersionResourcesResolutionStatusResponse_status' - The status of the action.
newDescribeAppVersionResourcesResolutionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  -- | 'resolutionId'
  Prelude.Text ->
  -- | 'status'
  ResourceResolutionStatusType ->
  DescribeAppVersionResourcesResolutionStatusResponse
newDescribeAppVersionResourcesResolutionStatusResponse
  pHttpStatus_
  pAppArn_
  pAppVersion_
  pResolutionId_
  pStatus_ =
    DescribeAppVersionResourcesResolutionStatusResponse'
      { errorMessage =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_,
        appArn = pAppArn_,
        appVersion =
          pAppVersion_,
        resolutionId =
          pResolutionId_,
        status = pStatus_
      }

-- | The returned error message for the request.
describeAppVersionResourcesResolutionStatusResponse_errorMessage :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse (Prelude.Maybe Prelude.Text)
describeAppVersionResourcesResolutionStatusResponse_errorMessage = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {errorMessage} -> errorMessage) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {errorMessage = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

-- | The response's http status code.
describeAppVersionResourcesResolutionStatusResponse_httpStatus :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse Prelude.Int
describeAppVersionResourcesResolutionStatusResponse_httpStatus = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {httpStatus = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAppVersionResourcesResolutionStatusResponse_appArn :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse Prelude.Text
describeAppVersionResourcesResolutionStatusResponse_appArn = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {appArn} -> appArn) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {appArn = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

-- | The version of the application.
describeAppVersionResourcesResolutionStatusResponse_appVersion :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse Prelude.Text
describeAppVersionResourcesResolutionStatusResponse_appVersion = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {appVersion} -> appVersion) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {appVersion = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

-- | The identifier for a specific resolution.
describeAppVersionResourcesResolutionStatusResponse_resolutionId :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse Prelude.Text
describeAppVersionResourcesResolutionStatusResponse_resolutionId = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {resolutionId} -> resolutionId) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {resolutionId = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

-- | The status of the action.
describeAppVersionResourcesResolutionStatusResponse_status :: Lens.Lens' DescribeAppVersionResourcesResolutionStatusResponse ResourceResolutionStatusType
describeAppVersionResourcesResolutionStatusResponse_status = Lens.lens (\DescribeAppVersionResourcesResolutionStatusResponse' {status} -> status) (\s@DescribeAppVersionResourcesResolutionStatusResponse' {} a -> s {status = a} :: DescribeAppVersionResourcesResolutionStatusResponse)

instance
  Prelude.NFData
    DescribeAppVersionResourcesResolutionStatusResponse
  where
  rnf
    DescribeAppVersionResourcesResolutionStatusResponse' {..} =
      Prelude.rnf errorMessage
        `Prelude.seq` Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf appArn
        `Prelude.seq` Prelude.rnf appVersion
        `Prelude.seq` Prelude.rnf resolutionId
        `Prelude.seq` Prelude.rnf status
