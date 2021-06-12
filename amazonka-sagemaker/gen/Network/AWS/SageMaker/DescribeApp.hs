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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { -- | The domain ID.
    domainId :: Core.Text,
    -- | The user profile name.
    userProfileName :: Core.Text,
    -- | The type of app.
    appType :: AppType,
    -- | The name of the app.
    appName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'userProfileName'
  Core.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Core.Text ->
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
describeApp_domainId :: Lens.Lens' DescribeApp Core.Text
describeApp_domainId = Lens.lens (\DescribeApp' {domainId} -> domainId) (\s@DescribeApp' {} a -> s {domainId = a} :: DescribeApp)

-- | The user profile name.
describeApp_userProfileName :: Lens.Lens' DescribeApp Core.Text
describeApp_userProfileName = Lens.lens (\DescribeApp' {userProfileName} -> userProfileName) (\s@DescribeApp' {} a -> s {userProfileName = a} :: DescribeApp)

-- | The type of app.
describeApp_appType :: Lens.Lens' DescribeApp AppType
describeApp_appType = Lens.lens (\DescribeApp' {appType} -> appType) (\s@DescribeApp' {} a -> s {appType = a} :: DescribeApp)

-- | The name of the app.
describeApp_appName :: Lens.Lens' DescribeApp Core.Text
describeApp_appName = Lens.lens (\DescribeApp' {appName} -> appName) (\s@DescribeApp' {} a -> s {appName = a} :: DescribeApp)

instance Core.AWSRequest DescribeApp where
  type AWSResponse DescribeApp = DescribeAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Core.<$> (x Core..?> "ResourceSpec")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "AppType")
            Core.<*> (x Core..?> "AppName")
            Core.<*> (x Core..?> "UserProfileName")
            Core.<*> (x Core..?> "DomainId")
            Core.<*> (x Core..?> "AppArn")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "LastHealthCheckTimestamp")
            Core.<*> (x Core..?> "LastUserActivityTimestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeApp

instance Core.NFData DescribeApp

instance Core.ToHeaders DescribeApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeApp" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeApp where
  toJSON DescribeApp' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            Core.Just
              ("UserProfileName" Core..= userProfileName),
            Core.Just ("AppType" Core..= appType),
            Core.Just ("AppName" Core..= appName)
          ]
      )

instance Core.ToPath DescribeApp where
  toPath = Core.const "/"

instance Core.ToQuery DescribeApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
    -- image created on the instance.
    resourceSpec :: Core.Maybe ResourceSpec,
    -- | The status.
    status :: Core.Maybe AppStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The type of app.
    appType :: Core.Maybe AppType,
    -- | The name of the app.
    appName :: Core.Maybe Core.Text,
    -- | The user profile name.
    userProfileName :: Core.Maybe Core.Text,
    -- | The domain ID.
    domainId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Core.Maybe Core.Text,
    -- | The failure reason.
    failureReason :: Core.Maybe Core.Text,
    -- | The timestamp of the last health check.
    lastHealthCheckTimestamp :: Core.Maybe Core.POSIX,
    -- | The timestamp of the last user\'s activity.
    lastUserActivityTimestamp :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAppResponse
newDescribeAppResponse pHttpStatus_ =
  DescribeAppResponse'
    { resourceSpec = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      appType = Core.Nothing,
      appName = Core.Nothing,
      userProfileName = Core.Nothing,
      domainId = Core.Nothing,
      appArn = Core.Nothing,
      failureReason = Core.Nothing,
      lastHealthCheckTimestamp = Core.Nothing,
      lastUserActivityTimestamp = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
describeAppResponse_resourceSpec :: Lens.Lens' DescribeAppResponse (Core.Maybe ResourceSpec)
describeAppResponse_resourceSpec = Lens.lens (\DescribeAppResponse' {resourceSpec} -> resourceSpec) (\s@DescribeAppResponse' {} a -> s {resourceSpec = a} :: DescribeAppResponse)

-- | The status.
describeAppResponse_status :: Lens.Lens' DescribeAppResponse (Core.Maybe AppStatus)
describeAppResponse_status = Lens.lens (\DescribeAppResponse' {status} -> status) (\s@DescribeAppResponse' {} a -> s {status = a} :: DescribeAppResponse)

-- | The creation time.
describeAppResponse_creationTime :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.UTCTime)
describeAppResponse_creationTime = Lens.lens (\DescribeAppResponse' {creationTime} -> creationTime) (\s@DescribeAppResponse' {} a -> s {creationTime = a} :: DescribeAppResponse) Core.. Lens.mapping Core._Time

-- | The type of app.
describeAppResponse_appType :: Lens.Lens' DescribeAppResponse (Core.Maybe AppType)
describeAppResponse_appType = Lens.lens (\DescribeAppResponse' {appType} -> appType) (\s@DescribeAppResponse' {} a -> s {appType = a} :: DescribeAppResponse)

-- | The name of the app.
describeAppResponse_appName :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.Text)
describeAppResponse_appName = Lens.lens (\DescribeAppResponse' {appName} -> appName) (\s@DescribeAppResponse' {} a -> s {appName = a} :: DescribeAppResponse)

-- | The user profile name.
describeAppResponse_userProfileName :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.Text)
describeAppResponse_userProfileName = Lens.lens (\DescribeAppResponse' {userProfileName} -> userProfileName) (\s@DescribeAppResponse' {} a -> s {userProfileName = a} :: DescribeAppResponse)

-- | The domain ID.
describeAppResponse_domainId :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.Text)
describeAppResponse_domainId = Lens.lens (\DescribeAppResponse' {domainId} -> domainId) (\s@DescribeAppResponse' {} a -> s {domainId = a} :: DescribeAppResponse)

-- | The Amazon Resource Name (ARN) of the app.
describeAppResponse_appArn :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.Text)
describeAppResponse_appArn = Lens.lens (\DescribeAppResponse' {appArn} -> appArn) (\s@DescribeAppResponse' {} a -> s {appArn = a} :: DescribeAppResponse)

-- | The failure reason.
describeAppResponse_failureReason :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.Text)
describeAppResponse_failureReason = Lens.lens (\DescribeAppResponse' {failureReason} -> failureReason) (\s@DescribeAppResponse' {} a -> s {failureReason = a} :: DescribeAppResponse)

-- | The timestamp of the last health check.
describeAppResponse_lastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.UTCTime)
describeAppResponse_lastHealthCheckTimestamp = Lens.lens (\DescribeAppResponse' {lastHealthCheckTimestamp} -> lastHealthCheckTimestamp) (\s@DescribeAppResponse' {} a -> s {lastHealthCheckTimestamp = a} :: DescribeAppResponse) Core.. Lens.mapping Core._Time

-- | The timestamp of the last user\'s activity.
describeAppResponse_lastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Core.Maybe Core.UTCTime)
describeAppResponse_lastUserActivityTimestamp = Lens.lens (\DescribeAppResponse' {lastUserActivityTimestamp} -> lastUserActivityTimestamp) (\s@DescribeAppResponse' {} a -> s {lastUserActivityTimestamp = a} :: DescribeAppResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeAppResponse_httpStatus :: Lens.Lens' DescribeAppResponse Core.Int
describeAppResponse_httpStatus = Lens.lens (\DescribeAppResponse' {httpStatus} -> httpStatus) (\s@DescribeAppResponse' {} a -> s {httpStatus = a} :: DescribeAppResponse)

instance Core.NFData DescribeAppResponse
