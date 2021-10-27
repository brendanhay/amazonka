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
-- Module      : Network.AWS.Backup.DescribeFramework
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the framework details for the specified @FrameworkName@.
module Network.AWS.Backup.DescribeFramework
  ( -- * Creating a Request
    DescribeFramework (..),
    newDescribeFramework,

    -- * Request Lenses
    describeFramework_frameworkName,

    -- * Destructuring the Response
    DescribeFrameworkResponse (..),
    newDescribeFrameworkResponse,

    -- * Response Lenses
    describeFrameworkResponse_creationTime,
    describeFrameworkResponse_idempotencyToken,
    describeFrameworkResponse_frameworkDescription,
    describeFrameworkResponse_frameworkStatus,
    describeFrameworkResponse_frameworkControls,
    describeFrameworkResponse_frameworkArn,
    describeFrameworkResponse_deploymentStatus,
    describeFrameworkResponse_frameworkName,
    describeFrameworkResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFramework' smart constructor.
data DescribeFramework = DescribeFramework'
  { -- | The unique name of a framework.
    frameworkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frameworkName', 'describeFramework_frameworkName' - The unique name of a framework.
newDescribeFramework ::
  -- | 'frameworkName'
  Prelude.Text ->
  DescribeFramework
newDescribeFramework pFrameworkName_ =
  DescribeFramework' {frameworkName = pFrameworkName_}

-- | The unique name of a framework.
describeFramework_frameworkName :: Lens.Lens' DescribeFramework Prelude.Text
describeFramework_frameworkName = Lens.lens (\DescribeFramework' {frameworkName} -> frameworkName) (\s@DescribeFramework' {} a -> s {frameworkName = a} :: DescribeFramework)

instance Core.AWSRequest DescribeFramework where
  type
    AWSResponse DescribeFramework =
      DescribeFrameworkResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFrameworkResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "IdempotencyToken")
            Prelude.<*> (x Core..?> "FrameworkDescription")
            Prelude.<*> (x Core..?> "FrameworkStatus")
            Prelude.<*> ( x Core..?> "FrameworkControls"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "FrameworkArn")
            Prelude.<*> (x Core..?> "DeploymentStatus")
            Prelude.<*> (x Core..?> "FrameworkName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFramework

instance Prelude.NFData DescribeFramework

instance Core.ToHeaders DescribeFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeFramework where
  toPath DescribeFramework' {..} =
    Prelude.mconcat
      ["/audit/frameworks/", Core.toBS frameworkName]

instance Core.ToQuery DescribeFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFrameworkResponse' smart constructor.
data DescribeFrameworkResponse = DescribeFrameworkResponse'
  { -- | The date and time that a framework is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationTime@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A customer-chosen string that you can use to distinguish between
    -- otherwise identical calls to @DescribeFrameworkOutput@. Retrying a
    -- successful request with the same idempotency token results in a success
    -- message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | An optional description of the framework.
    frameworkDescription :: Prelude.Maybe Prelude.Text,
    -- | A framework consists of one or more controls. Each control governs a
    -- resource, such as backup plans, backup selections, backup vaults, or
    -- recovery points. You can also turn Config recording on or off for each
    -- resource. The statuses are:
    --
    -- -   @ACTIVE@ when recording is turned on for all resources governed by
    --     the framework.
    --
    -- -   @PARTIALLY_ACTIVE@ when recording is turned off for at least one
    --     resource governed by the framework.
    --
    -- -   @INACTIVE@ when recording is turned off for all resources governed
    --     by the framework.
    --
    -- -   @UNAVAILABLE@ when Backup is unable to validate recording status at
    --     this time.
    frameworkStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of the controls that make up the framework. Each control in the
    -- list has a name, input parameters, and scope.
    frameworkControls :: Prelude.Maybe [FrameworkControl],
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    frameworkArn :: Prelude.Maybe Prelude.Text,
    -- | The deployment status of a framework. The statuses are:
    --
    -- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique name of a framework.
    frameworkName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeFrameworkResponse_creationTime' - The date and time that a framework is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'idempotencyToken', 'describeFrameworkResponse_idempotencyToken' - A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @DescribeFrameworkOutput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
--
-- 'frameworkDescription', 'describeFrameworkResponse_frameworkDescription' - An optional description of the framework.
--
-- 'frameworkStatus', 'describeFrameworkResponse_frameworkStatus' - A framework consists of one or more controls. Each control governs a
-- resource, such as backup plans, backup selections, backup vaults, or
-- recovery points. You can also turn Config recording on or off for each
-- resource. The statuses are:
--
-- -   @ACTIVE@ when recording is turned on for all resources governed by
--     the framework.
--
-- -   @PARTIALLY_ACTIVE@ when recording is turned off for at least one
--     resource governed by the framework.
--
-- -   @INACTIVE@ when recording is turned off for all resources governed
--     by the framework.
--
-- -   @UNAVAILABLE@ when Backup is unable to validate recording status at
--     this time.
--
-- 'frameworkControls', 'describeFrameworkResponse_frameworkControls' - A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
--
-- 'frameworkArn', 'describeFrameworkResponse_frameworkArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'deploymentStatus', 'describeFrameworkResponse_deploymentStatus' - The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
--
-- 'frameworkName', 'describeFrameworkResponse_frameworkName' - The unique name of a framework.
--
-- 'httpStatus', 'describeFrameworkResponse_httpStatus' - The response's http status code.
newDescribeFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFrameworkResponse
newDescribeFrameworkResponse pHttpStatus_ =
  DescribeFrameworkResponse'
    { creationTime =
        Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      frameworkDescription = Prelude.Nothing,
      frameworkStatus = Prelude.Nothing,
      frameworkControls = Prelude.Nothing,
      frameworkArn = Prelude.Nothing,
      deploymentStatus = Prelude.Nothing,
      frameworkName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that a framework is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationTime@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
describeFrameworkResponse_creationTime :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.UTCTime)
describeFrameworkResponse_creationTime = Lens.lens (\DescribeFrameworkResponse' {creationTime} -> creationTime) (\s@DescribeFrameworkResponse' {} a -> s {creationTime = a} :: DescribeFrameworkResponse) Prelude.. Lens.mapping Core._Time

-- | A customer-chosen string that you can use to distinguish between
-- otherwise identical calls to @DescribeFrameworkOutput@. Retrying a
-- successful request with the same idempotency token results in a success
-- message with no action taken.
describeFrameworkResponse_idempotencyToken :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_idempotencyToken = Lens.lens (\DescribeFrameworkResponse' {idempotencyToken} -> idempotencyToken) (\s@DescribeFrameworkResponse' {} a -> s {idempotencyToken = a} :: DescribeFrameworkResponse)

-- | An optional description of the framework.
describeFrameworkResponse_frameworkDescription :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_frameworkDescription = Lens.lens (\DescribeFrameworkResponse' {frameworkDescription} -> frameworkDescription) (\s@DescribeFrameworkResponse' {} a -> s {frameworkDescription = a} :: DescribeFrameworkResponse)

-- | A framework consists of one or more controls. Each control governs a
-- resource, such as backup plans, backup selections, backup vaults, or
-- recovery points. You can also turn Config recording on or off for each
-- resource. The statuses are:
--
-- -   @ACTIVE@ when recording is turned on for all resources governed by
--     the framework.
--
-- -   @PARTIALLY_ACTIVE@ when recording is turned off for at least one
--     resource governed by the framework.
--
-- -   @INACTIVE@ when recording is turned off for all resources governed
--     by the framework.
--
-- -   @UNAVAILABLE@ when Backup is unable to validate recording status at
--     this time.
describeFrameworkResponse_frameworkStatus :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_frameworkStatus = Lens.lens (\DescribeFrameworkResponse' {frameworkStatus} -> frameworkStatus) (\s@DescribeFrameworkResponse' {} a -> s {frameworkStatus = a} :: DescribeFrameworkResponse)

-- | A list of the controls that make up the framework. Each control in the
-- list has a name, input parameters, and scope.
describeFrameworkResponse_frameworkControls :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe [FrameworkControl])
describeFrameworkResponse_frameworkControls = Lens.lens (\DescribeFrameworkResponse' {frameworkControls} -> frameworkControls) (\s@DescribeFrameworkResponse' {} a -> s {frameworkControls = a} :: DescribeFrameworkResponse) Prelude.. Lens.mapping Lens.coerced

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
describeFrameworkResponse_frameworkArn :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_frameworkArn = Lens.lens (\DescribeFrameworkResponse' {frameworkArn} -> frameworkArn) (\s@DescribeFrameworkResponse' {} a -> s {frameworkArn = a} :: DescribeFrameworkResponse)

-- | The deployment status of a framework. The statuses are:
--
-- @CREATE_IN_PROGRESS | UPDATE_IN_PROGRESS | DELETE_IN_PROGRESS | COMPLETED | FAILED@
describeFrameworkResponse_deploymentStatus :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_deploymentStatus = Lens.lens (\DescribeFrameworkResponse' {deploymentStatus} -> deploymentStatus) (\s@DescribeFrameworkResponse' {} a -> s {deploymentStatus = a} :: DescribeFrameworkResponse)

-- | The unique name of a framework.
describeFrameworkResponse_frameworkName :: Lens.Lens' DescribeFrameworkResponse (Prelude.Maybe Prelude.Text)
describeFrameworkResponse_frameworkName = Lens.lens (\DescribeFrameworkResponse' {frameworkName} -> frameworkName) (\s@DescribeFrameworkResponse' {} a -> s {frameworkName = a} :: DescribeFrameworkResponse)

-- | The response's http status code.
describeFrameworkResponse_httpStatus :: Lens.Lens' DescribeFrameworkResponse Prelude.Int
describeFrameworkResponse_httpStatus = Lens.lens (\DescribeFrameworkResponse' {httpStatus} -> httpStatus) (\s@DescribeFrameworkResponse' {} a -> s {httpStatus = a} :: DescribeFrameworkResponse)

instance Prelude.NFData DescribeFrameworkResponse
