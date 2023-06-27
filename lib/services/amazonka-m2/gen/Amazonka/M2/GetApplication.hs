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
-- Module      : Amazonka.M2.GetApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a specific application.
module Amazonka.M2.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_applicationId,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_deployedVersion,
    getApplicationResponse_description,
    getApplicationResponse_environmentId,
    getApplicationResponse_kmsKeyId,
    getApplicationResponse_lastStartTime,
    getApplicationResponse_listenerArns,
    getApplicationResponse_listenerPorts,
    getApplicationResponse_loadBalancerDnsName,
    getApplicationResponse_logGroups,
    getApplicationResponse_roleArn,
    getApplicationResponse_statusReason,
    getApplicationResponse_tags,
    getApplicationResponse_targetGroupArns,
    getApplicationResponse_httpStatus,
    getApplicationResponse_applicationArn,
    getApplicationResponse_applicationId,
    getApplicationResponse_creationTime,
    getApplicationResponse_engineType,
    getApplicationResponse_latestVersion,
    getApplicationResponse_name,
    getApplicationResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The identifier of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApplication_applicationId' - The identifier of the application.
newGetApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApplication
newGetApplication pApplicationId_ =
  GetApplication' {applicationId = pApplicationId_}

-- | The identifier of the application.
getApplication_applicationId :: Lens.Lens' GetApplication Prelude.Text
getApplication_applicationId = Lens.lens (\GetApplication' {applicationId} -> applicationId) (\s@GetApplication' {} a -> s {applicationId = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Data..?> "deployedVersion")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "environmentId")
            Prelude.<*> (x Data..?> "kmsKeyId")
            Prelude.<*> (x Data..?> "lastStartTime")
            Prelude.<*> (x Data..?> "listenerArns")
            Prelude.<*> (x Data..?> "listenerPorts")
            Prelude.<*> (x Data..?> "loadBalancerDnsName")
            Prelude.<*> (x Data..?> "logGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "statusReason")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "targetGroupArns")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationArn")
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "engineType")
            Prelude.<*> (x Data..:> "latestVersion")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} = Prelude.rnf applicationId

instance Data.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplication where
  toPath GetApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery GetApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The version of the application that is deployed.
    deployedVersion :: Prelude.Maybe DeployedVersionSummary,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the runtime environment where you want to deploy the
    -- application.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a customer managed key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when you last started the application. Null until the
    -- application runs for the first time.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the network load balancer listener
    -- created in your Amazon Web Services account. Amazon Web Services
    -- Mainframe Modernization creates this listener for you the first time you
    -- deploy an application.
    listenerArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The port associated with the network load balancer listener created in
    -- your Amazon Web Services account.
    listenerPorts :: Prelude.Maybe (Prelude.NonEmpty Prelude.Int),
    -- | The public DNS name of the load balancer created in your Amazon Web
    -- Services account.
    loadBalancerDnsName :: Prelude.Maybe Prelude.Text,
    -- | The list of log summaries. Each log summary includes the log type as
    -- well as the log group identifier. These are CloudWatch logs. Amazon Web
    -- Services Mainframe Modernization pushes the application log to
    -- CloudWatch under the customer\'s account.
    logGroups :: Prelude.Maybe [LogGroupSummary],
    -- | The Amazon Resource Name (ARN) of the role associated with the
    -- application.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | A list of tags associated with the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Returns the Amazon Resource Names (ARNs) of the target groups that are
    -- attached to the network load balancer.
    targetGroupArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Text,
    -- | The identifier of the application.
    applicationId :: Prelude.Text,
    -- | The timestamp when this application was created.
    creationTime :: Data.POSIX,
    -- | The type of the target platform for the application.
    engineType :: EngineType,
    -- | The latest version of the application.
    latestVersion :: ApplicationVersionSummary,
    -- | The unique identifier of the application.
    name :: Prelude.Text,
    -- | The status of the application.
    status :: ApplicationLifecycle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployedVersion', 'getApplicationResponse_deployedVersion' - The version of the application that is deployed.
--
-- 'description', 'getApplicationResponse_description' - The description of the application.
--
-- 'environmentId', 'getApplicationResponse_environmentId' - The identifier of the runtime environment where you want to deploy the
-- application.
--
-- 'kmsKeyId', 'getApplicationResponse_kmsKeyId' - The identifier of a customer managed key.
--
-- 'lastStartTime', 'getApplicationResponse_lastStartTime' - The timestamp when you last started the application. Null until the
-- application runs for the first time.
--
-- 'listenerArns', 'getApplicationResponse_listenerArns' - The Amazon Resource Name (ARN) for the network load balancer listener
-- created in your Amazon Web Services account. Amazon Web Services
-- Mainframe Modernization creates this listener for you the first time you
-- deploy an application.
--
-- 'listenerPorts', 'getApplicationResponse_listenerPorts' - The port associated with the network load balancer listener created in
-- your Amazon Web Services account.
--
-- 'loadBalancerDnsName', 'getApplicationResponse_loadBalancerDnsName' - The public DNS name of the load balancer created in your Amazon Web
-- Services account.
--
-- 'logGroups', 'getApplicationResponse_logGroups' - The list of log summaries. Each log summary includes the log type as
-- well as the log group identifier. These are CloudWatch logs. Amazon Web
-- Services Mainframe Modernization pushes the application log to
-- CloudWatch under the customer\'s account.
--
-- 'roleArn', 'getApplicationResponse_roleArn' - The Amazon Resource Name (ARN) of the role associated with the
-- application.
--
-- 'statusReason', 'getApplicationResponse_statusReason' - The reason for the reported status.
--
-- 'tags', 'getApplicationResponse_tags' - A list of tags associated with the application.
--
-- 'targetGroupArns', 'getApplicationResponse_targetGroupArns' - Returns the Amazon Resource Names (ARNs) of the target groups that are
-- attached to the network load balancer.
--
-- 'httpStatus', 'getApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationArn', 'getApplicationResponse_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationId', 'getApplicationResponse_applicationId' - The identifier of the application.
--
-- 'creationTime', 'getApplicationResponse_creationTime' - The timestamp when this application was created.
--
-- 'engineType', 'getApplicationResponse_engineType' - The type of the target platform for the application.
--
-- 'latestVersion', 'getApplicationResponse_latestVersion' - The latest version of the application.
--
-- 'name', 'getApplicationResponse_name' - The unique identifier of the application.
--
-- 'status', 'getApplicationResponse_status' - The status of the application.
newGetApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationArn'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'engineType'
  EngineType ->
  -- | 'latestVersion'
  ApplicationVersionSummary ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ApplicationLifecycle ->
  GetApplicationResponse
newGetApplicationResponse
  pHttpStatus_
  pApplicationArn_
  pApplicationId_
  pCreationTime_
  pEngineType_
  pLatestVersion_
  pName_
  pStatus_ =
    GetApplicationResponse'
      { deployedVersion =
          Prelude.Nothing,
        description = Prelude.Nothing,
        environmentId = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        lastStartTime = Prelude.Nothing,
        listenerArns = Prelude.Nothing,
        listenerPorts = Prelude.Nothing,
        loadBalancerDnsName = Prelude.Nothing,
        logGroups = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetGroupArns = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationArn = pApplicationArn_,
        applicationId = pApplicationId_,
        creationTime = Data._Time Lens.# pCreationTime_,
        engineType = pEngineType_,
        latestVersion = pLatestVersion_,
        name = pName_,
        status = pStatus_
      }

-- | The version of the application that is deployed.
getApplicationResponse_deployedVersion :: Lens.Lens' GetApplicationResponse (Prelude.Maybe DeployedVersionSummary)
getApplicationResponse_deployedVersion = Lens.lens (\GetApplicationResponse' {deployedVersion} -> deployedVersion) (\s@GetApplicationResponse' {} a -> s {deployedVersion = a} :: GetApplicationResponse)

-- | The description of the application.
getApplicationResponse_description :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_description = Lens.lens (\GetApplicationResponse' {description} -> description) (\s@GetApplicationResponse' {} a -> s {description = a} :: GetApplicationResponse)

-- | The identifier of the runtime environment where you want to deploy the
-- application.
getApplicationResponse_environmentId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_environmentId = Lens.lens (\GetApplicationResponse' {environmentId} -> environmentId) (\s@GetApplicationResponse' {} a -> s {environmentId = a} :: GetApplicationResponse)

-- | The identifier of a customer managed key.
getApplicationResponse_kmsKeyId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_kmsKeyId = Lens.lens (\GetApplicationResponse' {kmsKeyId} -> kmsKeyId) (\s@GetApplicationResponse' {} a -> s {kmsKeyId = a} :: GetApplicationResponse)

-- | The timestamp when you last started the application. Null until the
-- application runs for the first time.
getApplicationResponse_lastStartTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.UTCTime)
getApplicationResponse_lastStartTime = Lens.lens (\GetApplicationResponse' {lastStartTime} -> lastStartTime) (\s@GetApplicationResponse' {} a -> s {lastStartTime = a} :: GetApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the network load balancer listener
-- created in your Amazon Web Services account. Amazon Web Services
-- Mainframe Modernization creates this listener for you the first time you
-- deploy an application.
getApplicationResponse_listenerArns :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getApplicationResponse_listenerArns = Lens.lens (\GetApplicationResponse' {listenerArns} -> listenerArns) (\s@GetApplicationResponse' {} a -> s {listenerArns = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The port associated with the network load balancer listener created in
-- your Amazon Web Services account.
getApplicationResponse_listenerPorts :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Int))
getApplicationResponse_listenerPorts = Lens.lens (\GetApplicationResponse' {listenerPorts} -> listenerPorts) (\s@GetApplicationResponse' {} a -> s {listenerPorts = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The public DNS name of the load balancer created in your Amazon Web
-- Services account.
getApplicationResponse_loadBalancerDnsName :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_loadBalancerDnsName = Lens.lens (\GetApplicationResponse' {loadBalancerDnsName} -> loadBalancerDnsName) (\s@GetApplicationResponse' {} a -> s {loadBalancerDnsName = a} :: GetApplicationResponse)

-- | The list of log summaries. Each log summary includes the log type as
-- well as the log group identifier. These are CloudWatch logs. Amazon Web
-- Services Mainframe Modernization pushes the application log to
-- CloudWatch under the customer\'s account.
getApplicationResponse_logGroups :: Lens.Lens' GetApplicationResponse (Prelude.Maybe [LogGroupSummary])
getApplicationResponse_logGroups = Lens.lens (\GetApplicationResponse' {logGroups} -> logGroups) (\s@GetApplicationResponse' {} a -> s {logGroups = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role associated with the
-- application.
getApplicationResponse_roleArn :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_roleArn = Lens.lens (\GetApplicationResponse' {roleArn} -> roleArn) (\s@GetApplicationResponse' {} a -> s {roleArn = a} :: GetApplicationResponse)

-- | The reason for the reported status.
getApplicationResponse_statusReason :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_statusReason = Lens.lens (\GetApplicationResponse' {statusReason} -> statusReason) (\s@GetApplicationResponse' {} a -> s {statusReason = a} :: GetApplicationResponse)

-- | A list of tags associated with the application.
getApplicationResponse_tags :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getApplicationResponse_tags = Lens.lens (\GetApplicationResponse' {tags} -> tags) (\s@GetApplicationResponse' {} a -> s {tags = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns the Amazon Resource Names (ARNs) of the target groups that are
-- attached to the network load balancer.
getApplicationResponse_targetGroupArns :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getApplicationResponse_targetGroupArns = Lens.lens (\GetApplicationResponse' {targetGroupArns} -> targetGroupArns) (\s@GetApplicationResponse' {} a -> s {targetGroupArns = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
getApplicationResponse_applicationArn :: Lens.Lens' GetApplicationResponse Prelude.Text
getApplicationResponse_applicationArn = Lens.lens (\GetApplicationResponse' {applicationArn} -> applicationArn) (\s@GetApplicationResponse' {} a -> s {applicationArn = a} :: GetApplicationResponse)

-- | The identifier of the application.
getApplicationResponse_applicationId :: Lens.Lens' GetApplicationResponse Prelude.Text
getApplicationResponse_applicationId = Lens.lens (\GetApplicationResponse' {applicationId} -> applicationId) (\s@GetApplicationResponse' {} a -> s {applicationId = a} :: GetApplicationResponse)

-- | The timestamp when this application was created.
getApplicationResponse_creationTime :: Lens.Lens' GetApplicationResponse Prelude.UTCTime
getApplicationResponse_creationTime = Lens.lens (\GetApplicationResponse' {creationTime} -> creationTime) (\s@GetApplicationResponse' {} a -> s {creationTime = a} :: GetApplicationResponse) Prelude.. Data._Time

-- | The type of the target platform for the application.
getApplicationResponse_engineType :: Lens.Lens' GetApplicationResponse EngineType
getApplicationResponse_engineType = Lens.lens (\GetApplicationResponse' {engineType} -> engineType) (\s@GetApplicationResponse' {} a -> s {engineType = a} :: GetApplicationResponse)

-- | The latest version of the application.
getApplicationResponse_latestVersion :: Lens.Lens' GetApplicationResponse ApplicationVersionSummary
getApplicationResponse_latestVersion = Lens.lens (\GetApplicationResponse' {latestVersion} -> latestVersion) (\s@GetApplicationResponse' {} a -> s {latestVersion = a} :: GetApplicationResponse)

-- | The unique identifier of the application.
getApplicationResponse_name :: Lens.Lens' GetApplicationResponse Prelude.Text
getApplicationResponse_name = Lens.lens (\GetApplicationResponse' {name} -> name) (\s@GetApplicationResponse' {} a -> s {name = a} :: GetApplicationResponse)

-- | The status of the application.
getApplicationResponse_status :: Lens.Lens' GetApplicationResponse ApplicationLifecycle
getApplicationResponse_status = Lens.lens (\GetApplicationResponse' {status} -> status) (\s@GetApplicationResponse' {} a -> s {status = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf deployedVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf listenerArns
      `Prelude.seq` Prelude.rnf listenerPorts
      `Prelude.seq` Prelude.rnf loadBalancerDnsName
      `Prelude.seq` Prelude.rnf logGroups
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetGroupArns
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
