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
-- Module      : Network.AWS.SageMaker.DescribeApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the app.
module Network.AWS.SageMaker.DescribeApp
  ( -- * Creating a Request
    DescribeApp (..),
    newDescribeApp,

    -- * Request Lenses
    describeApp_domainId,
    describeApp_userProfileName,
    describeApp_appType,
    describeApp_appName,

    -- * Destructuring the Response
    DescribeAppResponse (..),
    newDescribeAppResponse,

    -- * Response Lenses
    describeAppResponse_resourceSpec,
    describeAppResponse_status,
    describeAppResponse_creationTime,
    describeAppResponse_appType,
    describeAppResponse_appName,
    describeAppResponse_userProfileName,
    describeAppResponse_domainId,
    describeAppResponse_appArn,
    describeAppResponse_failureReason,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text,
    -- | The type of app.
    appType :: AppType,
    -- | The name of the app.
    appName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeApp_domainId' - The domain ID.
--
-- 'userProfileName', 'describeApp_userProfileName' - The user profile name.
--
-- 'appType', 'describeApp_appType' - The type of app.
--
-- 'appName', 'describeApp_appName' - The name of the app.
newDescribeApp ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Prelude.Text ->
  DescribeApp
newDescribeApp
  pDomainId_
  pUserProfileName_
  pAppType_
  pAppName_ =
    DescribeApp'
      { domainId = pDomainId_,
        userProfileName = pUserProfileName_,
        appType = pAppType_,
        appName = pAppName_
      }

-- | The domain ID.
describeApp_domainId :: Lens.Lens' DescribeApp Prelude.Text
describeApp_domainId = Lens.lens (\DescribeApp' {domainId} -> domainId) (\s@DescribeApp' {} a -> s {domainId = a} :: DescribeApp)

-- | The user profile name.
describeApp_userProfileName :: Lens.Lens' DescribeApp Prelude.Text
describeApp_userProfileName = Lens.lens (\DescribeApp' {userProfileName} -> userProfileName) (\s@DescribeApp' {} a -> s {userProfileName = a} :: DescribeApp)

-- | The type of app.
describeApp_appType :: Lens.Lens' DescribeApp AppType
describeApp_appType = Lens.lens (\DescribeApp' {appType} -> appType) (\s@DescribeApp' {} a -> s {appType = a} :: DescribeApp)

-- | The name of the app.
describeApp_appName :: Lens.Lens' DescribeApp Prelude.Text
describeApp_appName = Lens.lens (\DescribeApp' {appName} -> appName) (\s@DescribeApp' {} a -> s {appName = a} :: DescribeApp)

instance Core.AWSRequest DescribeApp where
  type AWSResponse DescribeApp = DescribeAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Prelude.<$> (x Core..?> "ResourceSpec")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "AppType")
            Prelude.<*> (x Core..?> "AppName")
            Prelude.<*> (x Core..?> "UserProfileName")
            Prelude.<*> (x Core..?> "DomainId")
            Prelude.<*> (x Core..?> "AppArn")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "LastHealthCheckTimestamp")
            Prelude.<*> (x Core..?> "LastUserActivityTimestamp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApp

instance Prelude.NFData DescribeApp

instance Core.ToHeaders DescribeApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeApp" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeApp where
  toJSON DescribeApp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just
              ("UserProfileName" Core..= userProfileName),
            Prelude.Just ("AppType" Core..= appType),
            Prelude.Just ("AppName" Core..= appName)
          ]
      )

instance Core.ToPath DescribeApp where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
    -- image created on the instance.
    resourceSpec :: Prelude.Maybe ResourceSpec,
    -- | The status.
    status :: Prelude.Maybe AppStatus,
    -- | The creation time.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The type of app.
    appType :: Prelude.Maybe AppType,
    -- | The name of the app.
    appName :: Prelude.Maybe Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last health check.
    lastHealthCheckTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The timestamp of the last user\'s activity.
    lastUserActivityTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpec', 'describeAppResponse_resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
--
-- 'status', 'describeAppResponse_status' - The status.
--
-- 'creationTime', 'describeAppResponse_creationTime' - The creation time.
--
-- 'appType', 'describeAppResponse_appType' - The type of app.
--
-- 'appName', 'describeAppResponse_appName' - The name of the app.
--
-- 'userProfileName', 'describeAppResponse_userProfileName' - The user profile name.
--
-- 'domainId', 'describeAppResponse_domainId' - The domain ID.
--
-- 'appArn', 'describeAppResponse_appArn' - The Amazon Resource Name (ARN) of the app.
--
-- 'failureReason', 'describeAppResponse_failureReason' - The failure reason.
--
-- 'lastHealthCheckTimestamp', 'describeAppResponse_lastHealthCheckTimestamp' - The timestamp of the last health check.
--
-- 'lastUserActivityTimestamp', 'describeAppResponse_lastUserActivityTimestamp' - The timestamp of the last user\'s activity.
--
-- 'httpStatus', 'describeAppResponse_httpStatus' - The response's http status code.
newDescribeAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppResponse
newDescribeAppResponse pHttpStatus_ =
  DescribeAppResponse'
    { resourceSpec =
        Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      appType = Prelude.Nothing,
      appName = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = Prelude.Nothing,
      appArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastHealthCheckTimestamp = Prelude.Nothing,
      lastUserActivityTimestamp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
describeAppResponse_resourceSpec :: Lens.Lens' DescribeAppResponse (Prelude.Maybe ResourceSpec)
describeAppResponse_resourceSpec = Lens.lens (\DescribeAppResponse' {resourceSpec} -> resourceSpec) (\s@DescribeAppResponse' {} a -> s {resourceSpec = a} :: DescribeAppResponse)

-- | The status.
describeAppResponse_status :: Lens.Lens' DescribeAppResponse (Prelude.Maybe AppStatus)
describeAppResponse_status = Lens.lens (\DescribeAppResponse' {status} -> status) (\s@DescribeAppResponse' {} a -> s {status = a} :: DescribeAppResponse)

-- | The creation time.
describeAppResponse_creationTime :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_creationTime = Lens.lens (\DescribeAppResponse' {creationTime} -> creationTime) (\s@DescribeAppResponse' {} a -> s {creationTime = a} :: DescribeAppResponse) Prelude.. Lens.mapping Core._Time

-- | The type of app.
describeAppResponse_appType :: Lens.Lens' DescribeAppResponse (Prelude.Maybe AppType)
describeAppResponse_appType = Lens.lens (\DescribeAppResponse' {appType} -> appType) (\s@DescribeAppResponse' {} a -> s {appType = a} :: DescribeAppResponse)

-- | The name of the app.
describeAppResponse_appName :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_appName = Lens.lens (\DescribeAppResponse' {appName} -> appName) (\s@DescribeAppResponse' {} a -> s {appName = a} :: DescribeAppResponse)

-- | The user profile name.
describeAppResponse_userProfileName :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_userProfileName = Lens.lens (\DescribeAppResponse' {userProfileName} -> userProfileName) (\s@DescribeAppResponse' {} a -> s {userProfileName = a} :: DescribeAppResponse)

-- | The domain ID.
describeAppResponse_domainId :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_domainId = Lens.lens (\DescribeAppResponse' {domainId} -> domainId) (\s@DescribeAppResponse' {} a -> s {domainId = a} :: DescribeAppResponse)

-- | The Amazon Resource Name (ARN) of the app.
describeAppResponse_appArn :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_appArn = Lens.lens (\DescribeAppResponse' {appArn} -> appArn) (\s@DescribeAppResponse' {} a -> s {appArn = a} :: DescribeAppResponse)

-- | The failure reason.
describeAppResponse_failureReason :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_failureReason = Lens.lens (\DescribeAppResponse' {failureReason} -> failureReason) (\s@DescribeAppResponse' {} a -> s {failureReason = a} :: DescribeAppResponse)

-- | The timestamp of the last health check.
describeAppResponse_lastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_lastHealthCheckTimestamp = Lens.lens (\DescribeAppResponse' {lastHealthCheckTimestamp} -> lastHealthCheckTimestamp) (\s@DescribeAppResponse' {} a -> s {lastHealthCheckTimestamp = a} :: DescribeAppResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp of the last user\'s activity.
describeAppResponse_lastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_lastUserActivityTimestamp = Lens.lens (\DescribeAppResponse' {lastUserActivityTimestamp} -> lastUserActivityTimestamp) (\s@DescribeAppResponse' {} a -> s {lastUserActivityTimestamp = a} :: DescribeAppResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeAppResponse_httpStatus :: Lens.Lens' DescribeAppResponse Prelude.Int
describeAppResponse_httpStatus = Lens.lens (\DescribeAppResponse' {httpStatus} -> httpStatus) (\s@DescribeAppResponse' {} a -> s {httpStatus = a} :: DescribeAppResponse)

instance Prelude.NFData DescribeAppResponse
