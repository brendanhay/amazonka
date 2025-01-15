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
-- Module      : Amazonka.SageMaker.DescribeApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the app.
module Amazonka.SageMaker.DescribeApp
  ( -- * Creating a Request
    DescribeApp (..),
    newDescribeApp,

    -- * Request Lenses
    describeApp_spaceName,
    describeApp_userProfileName,
    describeApp_domainId,
    describeApp_appType,
    describeApp_appName,

    -- * Destructuring the Response
    DescribeAppResponse (..),
    newDescribeAppResponse,

    -- * Response Lenses
    describeAppResponse_appArn,
    describeAppResponse_appName,
    describeAppResponse_appType,
    describeAppResponse_creationTime,
    describeAppResponse_domainId,
    describeAppResponse_failureReason,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_resourceSpec,
    describeAppResponse_spaceName,
    describeAppResponse_status,
    describeAppResponse_userProfileName,
    describeAppResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeApp' smart constructor.
data DescribeApp = DescribeApp'
  { -- | The name of the space.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | The user profile name. If this value is not set, then @SpaceName@ must
    -- be set.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Text,
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
-- 'spaceName', 'describeApp_spaceName' - The name of the space.
--
-- 'userProfileName', 'describeApp_userProfileName' - The user profile name. If this value is not set, then @SpaceName@ must
-- be set.
--
-- 'domainId', 'describeApp_domainId' - The domain ID.
--
-- 'appType', 'describeApp_appType' - The type of app.
--
-- 'appName', 'describeApp_appName' - The name of the app.
newDescribeApp ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Prelude.Text ->
  DescribeApp
newDescribeApp pDomainId_ pAppType_ pAppName_ =
  DescribeApp'
    { spaceName = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = pDomainId_,
      appType = pAppType_,
      appName = pAppName_
    }

-- | The name of the space.
describeApp_spaceName :: Lens.Lens' DescribeApp (Prelude.Maybe Prelude.Text)
describeApp_spaceName = Lens.lens (\DescribeApp' {spaceName} -> spaceName) (\s@DescribeApp' {} a -> s {spaceName = a} :: DescribeApp)

-- | The user profile name. If this value is not set, then @SpaceName@ must
-- be set.
describeApp_userProfileName :: Lens.Lens' DescribeApp (Prelude.Maybe Prelude.Text)
describeApp_userProfileName = Lens.lens (\DescribeApp' {userProfileName} -> userProfileName) (\s@DescribeApp' {} a -> s {userProfileName = a} :: DescribeApp)

-- | The domain ID.
describeApp_domainId :: Lens.Lens' DescribeApp Prelude.Text
describeApp_domainId = Lens.lens (\DescribeApp' {domainId} -> domainId) (\s@DescribeApp' {} a -> s {domainId = a} :: DescribeApp)

-- | The type of app.
describeApp_appType :: Lens.Lens' DescribeApp AppType
describeApp_appType = Lens.lens (\DescribeApp' {appType} -> appType) (\s@DescribeApp' {} a -> s {appType = a} :: DescribeApp)

-- | The name of the app.
describeApp_appName :: Lens.Lens' DescribeApp Prelude.Text
describeApp_appName = Lens.lens (\DescribeApp' {appName} -> appName) (\s@DescribeApp' {} a -> s {appName = a} :: DescribeApp)

instance Core.AWSRequest DescribeApp where
  type AWSResponse DescribeApp = DescribeAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppResponse'
            Prelude.<$> (x Data..?> "AppArn")
            Prelude.<*> (x Data..?> "AppName")
            Prelude.<*> (x Data..?> "AppType")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DomainId")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "LastHealthCheckTimestamp")
            Prelude.<*> (x Data..?> "LastUserActivityTimestamp")
            Prelude.<*> (x Data..?> "ResourceSpec")
            Prelude.<*> (x Data..?> "SpaceName")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "UserProfileName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApp where
  hashWithSalt _salt DescribeApp' {..} =
    _salt
      `Prelude.hashWithSalt` spaceName
      `Prelude.hashWithSalt` userProfileName
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` appName

instance Prelude.NFData DescribeApp where
  rnf DescribeApp' {..} =
    Prelude.rnf spaceName `Prelude.seq`
      Prelude.rnf userProfileName `Prelude.seq`
        Prelude.rnf domainId `Prelude.seq`
          Prelude.rnf appType `Prelude.seq`
            Prelude.rnf appName

instance Data.ToHeaders DescribeApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeApp" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeApp where
  toJSON DescribeApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpaceName" Data..=) Prelude.<$> spaceName,
            ("UserProfileName" Data..=)
              Prelude.<$> userProfileName,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("AppType" Data..= appType),
            Prelude.Just ("AppName" Data..= appName)
          ]
      )

instance Data.ToPath DescribeApp where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppResponse' smart constructor.
data DescribeAppResponse = DescribeAppResponse'
  { -- | The Amazon Resource Name (ARN) of the app.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the app.
    appName :: Prelude.Maybe Prelude.Text,
    -- | The type of app.
    appType :: Prelude.Maybe AppType,
    -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last health check.
    lastHealthCheckTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The timestamp of the last user\'s activity. @LastUserActivityTimestamp@
    -- is also updated when SageMaker performs health checks without user
    -- activity. As a result, this value is set to the same value as
    -- @LastHealthCheckTimestamp@.
    lastUserActivityTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
    -- image created on the instance.
    resourceSpec :: Prelude.Maybe ResourceSpec,
    -- | The name of the space. If this value is not set, then @UserProfileName@
    -- must be set.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe AppStatus,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
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
-- 'appArn', 'describeAppResponse_appArn' - The Amazon Resource Name (ARN) of the app.
--
-- 'appName', 'describeAppResponse_appName' - The name of the app.
--
-- 'appType', 'describeAppResponse_appType' - The type of app.
--
-- 'creationTime', 'describeAppResponse_creationTime' - The creation time.
--
-- 'domainId', 'describeAppResponse_domainId' - The domain ID.
--
-- 'failureReason', 'describeAppResponse_failureReason' - The failure reason.
--
-- 'lastHealthCheckTimestamp', 'describeAppResponse_lastHealthCheckTimestamp' - The timestamp of the last health check.
--
-- 'lastUserActivityTimestamp', 'describeAppResponse_lastUserActivityTimestamp' - The timestamp of the last user\'s activity. @LastUserActivityTimestamp@
-- is also updated when SageMaker performs health checks without user
-- activity. As a result, this value is set to the same value as
-- @LastHealthCheckTimestamp@.
--
-- 'resourceSpec', 'describeAppResponse_resourceSpec' - The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
--
-- 'spaceName', 'describeAppResponse_spaceName' - The name of the space. If this value is not set, then @UserProfileName@
-- must be set.
--
-- 'status', 'describeAppResponse_status' - The status.
--
-- 'userProfileName', 'describeAppResponse_userProfileName' - The user profile name.
--
-- 'httpStatus', 'describeAppResponse_httpStatus' - The response's http status code.
newDescribeAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppResponse
newDescribeAppResponse pHttpStatus_ =
  DescribeAppResponse'
    { appArn = Prelude.Nothing,
      appName = Prelude.Nothing,
      appType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastHealthCheckTimestamp = Prelude.Nothing,
      lastUserActivityTimestamp = Prelude.Nothing,
      resourceSpec = Prelude.Nothing,
      spaceName = Prelude.Nothing,
      status = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the app.
describeAppResponse_appArn :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_appArn = Lens.lens (\DescribeAppResponse' {appArn} -> appArn) (\s@DescribeAppResponse' {} a -> s {appArn = a} :: DescribeAppResponse)

-- | The name of the app.
describeAppResponse_appName :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_appName = Lens.lens (\DescribeAppResponse' {appName} -> appName) (\s@DescribeAppResponse' {} a -> s {appName = a} :: DescribeAppResponse)

-- | The type of app.
describeAppResponse_appType :: Lens.Lens' DescribeAppResponse (Prelude.Maybe AppType)
describeAppResponse_appType = Lens.lens (\DescribeAppResponse' {appType} -> appType) (\s@DescribeAppResponse' {} a -> s {appType = a} :: DescribeAppResponse)

-- | The creation time.
describeAppResponse_creationTime :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_creationTime = Lens.lens (\DescribeAppResponse' {creationTime} -> creationTime) (\s@DescribeAppResponse' {} a -> s {creationTime = a} :: DescribeAppResponse) Prelude.. Lens.mapping Data._Time

-- | The domain ID.
describeAppResponse_domainId :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_domainId = Lens.lens (\DescribeAppResponse' {domainId} -> domainId) (\s@DescribeAppResponse' {} a -> s {domainId = a} :: DescribeAppResponse)

-- | The failure reason.
describeAppResponse_failureReason :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_failureReason = Lens.lens (\DescribeAppResponse' {failureReason} -> failureReason) (\s@DescribeAppResponse' {} a -> s {failureReason = a} :: DescribeAppResponse)

-- | The timestamp of the last health check.
describeAppResponse_lastHealthCheckTimestamp :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_lastHealthCheckTimestamp = Lens.lens (\DescribeAppResponse' {lastHealthCheckTimestamp} -> lastHealthCheckTimestamp) (\s@DescribeAppResponse' {} a -> s {lastHealthCheckTimestamp = a} :: DescribeAppResponse) Prelude.. Lens.mapping Data._Time

-- | The timestamp of the last user\'s activity. @LastUserActivityTimestamp@
-- is also updated when SageMaker performs health checks without user
-- activity. As a result, this value is set to the same value as
-- @LastHealthCheckTimestamp@.
describeAppResponse_lastUserActivityTimestamp :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.UTCTime)
describeAppResponse_lastUserActivityTimestamp = Lens.lens (\DescribeAppResponse' {lastUserActivityTimestamp} -> lastUserActivityTimestamp) (\s@DescribeAppResponse' {} a -> s {lastUserActivityTimestamp = a} :: DescribeAppResponse) Prelude.. Lens.mapping Data._Time

-- | The instance type and the Amazon Resource Name (ARN) of the SageMaker
-- image created on the instance.
describeAppResponse_resourceSpec :: Lens.Lens' DescribeAppResponse (Prelude.Maybe ResourceSpec)
describeAppResponse_resourceSpec = Lens.lens (\DescribeAppResponse' {resourceSpec} -> resourceSpec) (\s@DescribeAppResponse' {} a -> s {resourceSpec = a} :: DescribeAppResponse)

-- | The name of the space. If this value is not set, then @UserProfileName@
-- must be set.
describeAppResponse_spaceName :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_spaceName = Lens.lens (\DescribeAppResponse' {spaceName} -> spaceName) (\s@DescribeAppResponse' {} a -> s {spaceName = a} :: DescribeAppResponse)

-- | The status.
describeAppResponse_status :: Lens.Lens' DescribeAppResponse (Prelude.Maybe AppStatus)
describeAppResponse_status = Lens.lens (\DescribeAppResponse' {status} -> status) (\s@DescribeAppResponse' {} a -> s {status = a} :: DescribeAppResponse)

-- | The user profile name.
describeAppResponse_userProfileName :: Lens.Lens' DescribeAppResponse (Prelude.Maybe Prelude.Text)
describeAppResponse_userProfileName = Lens.lens (\DescribeAppResponse' {userProfileName} -> userProfileName) (\s@DescribeAppResponse' {} a -> s {userProfileName = a} :: DescribeAppResponse)

-- | The response's http status code.
describeAppResponse_httpStatus :: Lens.Lens' DescribeAppResponse Prelude.Int
describeAppResponse_httpStatus = Lens.lens (\DescribeAppResponse' {httpStatus} -> httpStatus) (\s@DescribeAppResponse' {} a -> s {httpStatus = a} :: DescribeAppResponse)

instance Prelude.NFData DescribeAppResponse where
  rnf DescribeAppResponse' {..} =
    Prelude.rnf appArn `Prelude.seq`
      Prelude.rnf appName `Prelude.seq`
        Prelude.rnf appType `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf domainId `Prelude.seq`
              Prelude.rnf failureReason `Prelude.seq`
                Prelude.rnf lastHealthCheckTimestamp `Prelude.seq`
                  Prelude.rnf lastUserActivityTimestamp `Prelude.seq`
                    Prelude.rnf resourceSpec `Prelude.seq`
                      Prelude.rnf spaceName `Prelude.seq`
                        Prelude.rnf status `Prelude.seq`
                          Prelude.rnf userProfileName `Prelude.seq`
                            Prelude.rnf httpStatus
