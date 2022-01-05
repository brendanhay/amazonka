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
-- Module      : Amazonka.Panorama.DescribeApplicationInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an application instance on a device.
module Amazonka.Panorama.DescribeApplicationInstance
  ( -- * Creating a Request
    DescribeApplicationInstance (..),
    newDescribeApplicationInstance,

    -- * Request Lenses
    describeApplicationInstance_applicationInstanceId,

    -- * Destructuring the Response
    DescribeApplicationInstanceResponse (..),
    newDescribeApplicationInstanceResponse,

    -- * Response Lenses
    describeApplicationInstanceResponse_status,
    describeApplicationInstanceResponse_statusDescription,
    describeApplicationInstanceResponse_lastUpdatedTime,
    describeApplicationInstanceResponse_arn,
    describeApplicationInstanceResponse_createdTime,
    describeApplicationInstanceResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceResponse_defaultRuntimeContextDeviceName,
    describeApplicationInstanceResponse_name,
    describeApplicationInstanceResponse_runtimeRoleArn,
    describeApplicationInstanceResponse_healthStatus,
    describeApplicationInstanceResponse_applicationInstanceId,
    describeApplicationInstanceResponse_description,
    describeApplicationInstanceResponse_tags,
    describeApplicationInstanceResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplicationInstance' smart constructor.
data DescribeApplicationInstance = DescribeApplicationInstance'
  { -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'describeApplicationInstance_applicationInstanceId' - The application instance\'s ID.
newDescribeApplicationInstance ::
  -- | 'applicationInstanceId'
  Prelude.Text ->
  DescribeApplicationInstance
newDescribeApplicationInstance
  pApplicationInstanceId_ =
    DescribeApplicationInstance'
      { applicationInstanceId =
          pApplicationInstanceId_
      }

-- | The application instance\'s ID.
describeApplicationInstance_applicationInstanceId :: Lens.Lens' DescribeApplicationInstance Prelude.Text
describeApplicationInstance_applicationInstanceId = Lens.lens (\DescribeApplicationInstance' {applicationInstanceId} -> applicationInstanceId) (\s@DescribeApplicationInstance' {} a -> s {applicationInstanceId = a} :: DescribeApplicationInstance)

instance Core.AWSRequest DescribeApplicationInstance where
  type
    AWSResponse DescribeApplicationInstance =
      DescribeApplicationInstanceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationInstanceResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "StatusDescription")
            Prelude.<*> (x Core..?> "LastUpdatedTime")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "DefaultRuntimeContextDevice")
            Prelude.<*> (x Core..?> "DefaultRuntimeContextDeviceName")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "RuntimeRoleArn")
            Prelude.<*> (x Core..?> "HealthStatus")
            Prelude.<*> (x Core..?> "ApplicationInstanceId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ApplicationInstanceIdToReplace")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeApplicationInstance where
  hashWithSalt _salt DescribeApplicationInstance' {..} =
    _salt `Prelude.hashWithSalt` applicationInstanceId

instance Prelude.NFData DescribeApplicationInstance where
  rnf DescribeApplicationInstance' {..} =
    Prelude.rnf applicationInstanceId

instance Core.ToHeaders DescribeApplicationInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeApplicationInstance where
  toPath DescribeApplicationInstance' {..} =
    Prelude.mconcat
      [ "/application-instances/",
        Core.toBS applicationInstanceId
      ]

instance Core.ToQuery DescribeApplicationInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationInstanceResponse' smart constructor.
data DescribeApplicationInstanceResponse = DescribeApplicationInstanceResponse'
  { -- | The application instance\'s status.
    status :: Prelude.Maybe ApplicationInstanceStatus,
    -- | The application instance\'s status description.
    statusDescription :: Prelude.Maybe Prelude.Text,
    -- | The application instance was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The application instance\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the application instance was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The device\'s ID.
    defaultRuntimeContextDevice :: Prelude.Maybe Prelude.Text,
    -- | The device\'s bane.
    defaultRuntimeContextDeviceName :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s runtime role ARN.
    runtimeRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s health status.
    healthStatus :: Prelude.Maybe ApplicationInstanceHealthStatus,
    -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the application instance that this instance replaced.
    applicationInstanceIdToReplace :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeApplicationInstanceResponse_status' - The application instance\'s status.
--
-- 'statusDescription', 'describeApplicationInstanceResponse_statusDescription' - The application instance\'s status description.
--
-- 'lastUpdatedTime', 'describeApplicationInstanceResponse_lastUpdatedTime' - The application instance was updated.
--
-- 'arn', 'describeApplicationInstanceResponse_arn' - The application instance\'s ARN.
--
-- 'createdTime', 'describeApplicationInstanceResponse_createdTime' - When the application instance was created.
--
-- 'defaultRuntimeContextDevice', 'describeApplicationInstanceResponse_defaultRuntimeContextDevice' - The device\'s ID.
--
-- 'defaultRuntimeContextDeviceName', 'describeApplicationInstanceResponse_defaultRuntimeContextDeviceName' - The device\'s bane.
--
-- 'name', 'describeApplicationInstanceResponse_name' - The application instance\'s name.
--
-- 'runtimeRoleArn', 'describeApplicationInstanceResponse_runtimeRoleArn' - The application instance\'s runtime role ARN.
--
-- 'healthStatus', 'describeApplicationInstanceResponse_healthStatus' - The application instance\'s health status.
--
-- 'applicationInstanceId', 'describeApplicationInstanceResponse_applicationInstanceId' - The application instance\'s ID.
--
-- 'description', 'describeApplicationInstanceResponse_description' - The application instance\'s description.
--
-- 'tags', 'describeApplicationInstanceResponse_tags' - The application instance\'s tags.
--
-- 'applicationInstanceIdToReplace', 'describeApplicationInstanceResponse_applicationInstanceIdToReplace' - The ID of the application instance that this instance replaced.
--
-- 'httpStatus', 'describeApplicationInstanceResponse_httpStatus' - The response's http status code.
newDescribeApplicationInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationInstanceResponse
newDescribeApplicationInstanceResponse pHttpStatus_ =
  DescribeApplicationInstanceResponse'
    { status =
        Prelude.Nothing,
      statusDescription = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      defaultRuntimeContextDevice =
        Prelude.Nothing,
      defaultRuntimeContextDeviceName =
        Prelude.Nothing,
      name = Prelude.Nothing,
      runtimeRoleArn = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      applicationInstanceId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      applicationInstanceIdToReplace =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application instance\'s status.
describeApplicationInstanceResponse_status :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe ApplicationInstanceStatus)
describeApplicationInstanceResponse_status = Lens.lens (\DescribeApplicationInstanceResponse' {status} -> status) (\s@DescribeApplicationInstanceResponse' {} a -> s {status = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s status description.
describeApplicationInstanceResponse_statusDescription :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_statusDescription = Lens.lens (\DescribeApplicationInstanceResponse' {statusDescription} -> statusDescription) (\s@DescribeApplicationInstanceResponse' {} a -> s {statusDescription = a} :: DescribeApplicationInstanceResponse)

-- | The application instance was updated.
describeApplicationInstanceResponse_lastUpdatedTime :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.UTCTime)
describeApplicationInstanceResponse_lastUpdatedTime = Lens.lens (\DescribeApplicationInstanceResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DescribeApplicationInstanceResponse' {} a -> s {lastUpdatedTime = a} :: DescribeApplicationInstanceResponse) Prelude.. Lens.mapping Core._Time

-- | The application instance\'s ARN.
describeApplicationInstanceResponse_arn :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_arn = Lens.lens (\DescribeApplicationInstanceResponse' {arn} -> arn) (\s@DescribeApplicationInstanceResponse' {} a -> s {arn = a} :: DescribeApplicationInstanceResponse)

-- | When the application instance was created.
describeApplicationInstanceResponse_createdTime :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.UTCTime)
describeApplicationInstanceResponse_createdTime = Lens.lens (\DescribeApplicationInstanceResponse' {createdTime} -> createdTime) (\s@DescribeApplicationInstanceResponse' {} a -> s {createdTime = a} :: DescribeApplicationInstanceResponse) Prelude.. Lens.mapping Core._Time

-- | The device\'s ID.
describeApplicationInstanceResponse_defaultRuntimeContextDevice :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_defaultRuntimeContextDevice = Lens.lens (\DescribeApplicationInstanceResponse' {defaultRuntimeContextDevice} -> defaultRuntimeContextDevice) (\s@DescribeApplicationInstanceResponse' {} a -> s {defaultRuntimeContextDevice = a} :: DescribeApplicationInstanceResponse)

-- | The device\'s bane.
describeApplicationInstanceResponse_defaultRuntimeContextDeviceName :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_defaultRuntimeContextDeviceName = Lens.lens (\DescribeApplicationInstanceResponse' {defaultRuntimeContextDeviceName} -> defaultRuntimeContextDeviceName) (\s@DescribeApplicationInstanceResponse' {} a -> s {defaultRuntimeContextDeviceName = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s name.
describeApplicationInstanceResponse_name :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_name = Lens.lens (\DescribeApplicationInstanceResponse' {name} -> name) (\s@DescribeApplicationInstanceResponse' {} a -> s {name = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s runtime role ARN.
describeApplicationInstanceResponse_runtimeRoleArn :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_runtimeRoleArn = Lens.lens (\DescribeApplicationInstanceResponse' {runtimeRoleArn} -> runtimeRoleArn) (\s@DescribeApplicationInstanceResponse' {} a -> s {runtimeRoleArn = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s health status.
describeApplicationInstanceResponse_healthStatus :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe ApplicationInstanceHealthStatus)
describeApplicationInstanceResponse_healthStatus = Lens.lens (\DescribeApplicationInstanceResponse' {healthStatus} -> healthStatus) (\s@DescribeApplicationInstanceResponse' {} a -> s {healthStatus = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s ID.
describeApplicationInstanceResponse_applicationInstanceId :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_applicationInstanceId = Lens.lens (\DescribeApplicationInstanceResponse' {applicationInstanceId} -> applicationInstanceId) (\s@DescribeApplicationInstanceResponse' {} a -> s {applicationInstanceId = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s description.
describeApplicationInstanceResponse_description :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_description = Lens.lens (\DescribeApplicationInstanceResponse' {description} -> description) (\s@DescribeApplicationInstanceResponse' {} a -> s {description = a} :: DescribeApplicationInstanceResponse)

-- | The application instance\'s tags.
describeApplicationInstanceResponse_tags :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeApplicationInstanceResponse_tags = Lens.lens (\DescribeApplicationInstanceResponse' {tags} -> tags) (\s@DescribeApplicationInstanceResponse' {} a -> s {tags = a} :: DescribeApplicationInstanceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application instance that this instance replaced.
describeApplicationInstanceResponse_applicationInstanceIdToReplace :: Lens.Lens' DescribeApplicationInstanceResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceResponse_applicationInstanceIdToReplace = Lens.lens (\DescribeApplicationInstanceResponse' {applicationInstanceIdToReplace} -> applicationInstanceIdToReplace) (\s@DescribeApplicationInstanceResponse' {} a -> s {applicationInstanceIdToReplace = a} :: DescribeApplicationInstanceResponse)

-- | The response's http status code.
describeApplicationInstanceResponse_httpStatus :: Lens.Lens' DescribeApplicationInstanceResponse Prelude.Int
describeApplicationInstanceResponse_httpStatus = Lens.lens (\DescribeApplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationInstanceResponse' {} a -> s {httpStatus = a} :: DescribeApplicationInstanceResponse)

instance
  Prelude.NFData
    DescribeApplicationInstanceResponse
  where
  rnf DescribeApplicationInstanceResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusDescription
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf defaultRuntimeContextDevice
      `Prelude.seq` Prelude.rnf defaultRuntimeContextDeviceName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf runtimeRoleArn
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf applicationInstanceId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applicationInstanceIdToReplace
      `Prelude.seq` Prelude.rnf httpStatus
