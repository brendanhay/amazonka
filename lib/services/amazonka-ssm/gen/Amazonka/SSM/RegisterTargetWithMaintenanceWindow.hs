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
-- Module      : Amazonka.SSM.RegisterTargetWithMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a target with a maintenance window.
module Amazonka.SSM.RegisterTargetWithMaintenanceWindow
  ( -- * Creating a Request
    RegisterTargetWithMaintenanceWindow (..),
    newRegisterTargetWithMaintenanceWindow,

    -- * Request Lenses
    registerTargetWithMaintenanceWindow_clientToken,
    registerTargetWithMaintenanceWindow_description,
    registerTargetWithMaintenanceWindow_name,
    registerTargetWithMaintenanceWindow_ownerInformation,
    registerTargetWithMaintenanceWindow_windowId,
    registerTargetWithMaintenanceWindow_resourceType,
    registerTargetWithMaintenanceWindow_targets,

    -- * Destructuring the Response
    RegisterTargetWithMaintenanceWindowResponse (..),
    newRegisterTargetWithMaintenanceWindowResponse,

    -- * Response Lenses
    registerTargetWithMaintenanceWindowResponse_windowTargetId,
    registerTargetWithMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newRegisterTargetWithMaintenanceWindow' smart constructor.
data RegisterTargetWithMaintenanceWindow = RegisterTargetWithMaintenanceWindow'
  { -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An optional description for the target.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An optional name for the target.
    name :: Prelude.Maybe Prelude.Text,
    -- | User-provided value that will be included in any Amazon CloudWatch
    -- Events events raised while running tasks for these targets in this
    -- maintenance window.
    ownerInformation :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the maintenance window the target should be registered with.
    windowId :: Prelude.Text,
    -- | The type of target being registered with the maintenance window.
    resourceType :: MaintenanceWindowResourceType,
    -- | The targets to register with the maintenance window. In other words, the
    -- managed nodes to run commands on when the maintenance window runs.
    --
    -- If a single maintenance window task is registered with multiple targets,
    -- its task invocations occur sequentially and not in parallel. If your
    -- task must run on multiple targets at the same time, register a task for
    -- each target individually and assign each task the same priority level.
    --
    -- You can specify targets using managed node IDs, resource group names, or
    -- tags that have been applied to managed nodes.
    --
    -- __Example 1__: Specify managed node IDs
    --
    -- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>,\<instance-id-3>@
    --
    -- __Example 2__: Use tag key-pairs applied to managed nodes
    --
    -- @Key=tag:\<my-tag-key>,Values=\<my-tag-value-1>,\<my-tag-value-2>@
    --
    -- __Example 3__: Use tag-keys applied to managed nodes
    --
    -- @Key=tag-key,Values=\<my-tag-key-1>,\<my-tag-key-2>@
    --
    -- __Example 4__: Use resource group names
    --
    -- @Key=resource-groups:Name,Values=\<resource-group-name>@
    --
    -- __Example 5__: Use filters for resource group types
    --
    -- @Key=resource-groups:ResourceTypeFilters,Values=\<resource-type-1>,\<resource-type-2>@
    --
    -- For @Key=resource-groups:ResourceTypeFilters@, specify resource types in
    -- the following format
    --
    -- @Key=resource-groups:ResourceTypeFilters,Values=AWS::EC2::INSTANCE,AWS::EC2::VPC@
    --
    -- For more information about these examples formats, including the best
    -- use case for each one, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    targets :: [Target]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTargetWithMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'registerTargetWithMaintenanceWindow_clientToken' - User-provided idempotency token.
--
-- 'description', 'registerTargetWithMaintenanceWindow_description' - An optional description for the target.
--
-- 'name', 'registerTargetWithMaintenanceWindow_name' - An optional name for the target.
--
-- 'ownerInformation', 'registerTargetWithMaintenanceWindow_ownerInformation' - User-provided value that will be included in any Amazon CloudWatch
-- Events events raised while running tasks for these targets in this
-- maintenance window.
--
-- 'windowId', 'registerTargetWithMaintenanceWindow_windowId' - The ID of the maintenance window the target should be registered with.
--
-- 'resourceType', 'registerTargetWithMaintenanceWindow_resourceType' - The type of target being registered with the maintenance window.
--
-- 'targets', 'registerTargetWithMaintenanceWindow_targets' - The targets to register with the maintenance window. In other words, the
-- managed nodes to run commands on when the maintenance window runs.
--
-- If a single maintenance window task is registered with multiple targets,
-- its task invocations occur sequentially and not in parallel. If your
-- task must run on multiple targets at the same time, register a task for
-- each target individually and assign each task the same priority level.
--
-- You can specify targets using managed node IDs, resource group names, or
-- tags that have been applied to managed nodes.
--
-- __Example 1__: Specify managed node IDs
--
-- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>,\<instance-id-3>@
--
-- __Example 2__: Use tag key-pairs applied to managed nodes
--
-- @Key=tag:\<my-tag-key>,Values=\<my-tag-value-1>,\<my-tag-value-2>@
--
-- __Example 3__: Use tag-keys applied to managed nodes
--
-- @Key=tag-key,Values=\<my-tag-key-1>,\<my-tag-key-2>@
--
-- __Example 4__: Use resource group names
--
-- @Key=resource-groups:Name,Values=\<resource-group-name>@
--
-- __Example 5__: Use filters for resource group types
--
-- @Key=resource-groups:ResourceTypeFilters,Values=\<resource-type-1>,\<resource-type-2>@
--
-- For @Key=resource-groups:ResourceTypeFilters@, specify resource types in
-- the following format
--
-- @Key=resource-groups:ResourceTypeFilters,Values=AWS::EC2::INSTANCE,AWS::EC2::VPC@
--
-- For more information about these examples formats, including the best
-- use case for each one, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window>
-- in the /Amazon Web Services Systems Manager User Guide/.
newRegisterTargetWithMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'resourceType'
  MaintenanceWindowResourceType ->
  RegisterTargetWithMaintenanceWindow
newRegisterTargetWithMaintenanceWindow
  pWindowId_
  pResourceType_ =
    RegisterTargetWithMaintenanceWindow'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        name = Prelude.Nothing,
        ownerInformation = Prelude.Nothing,
        windowId = pWindowId_,
        resourceType = pResourceType_,
        targets = Prelude.mempty
      }

-- | User-provided idempotency token.
registerTargetWithMaintenanceWindow_clientToken :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTargetWithMaintenanceWindow_clientToken = Lens.lens (\RegisterTargetWithMaintenanceWindow' {clientToken} -> clientToken) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {clientToken = a} :: RegisterTargetWithMaintenanceWindow)

-- | An optional description for the target.
registerTargetWithMaintenanceWindow_description :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTargetWithMaintenanceWindow_description = Lens.lens (\RegisterTargetWithMaintenanceWindow' {description} -> description) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {description = a} :: RegisterTargetWithMaintenanceWindow) Prelude.. Lens.mapping Data._Sensitive

-- | An optional name for the target.
registerTargetWithMaintenanceWindow_name :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTargetWithMaintenanceWindow_name = Lens.lens (\RegisterTargetWithMaintenanceWindow' {name} -> name) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {name = a} :: RegisterTargetWithMaintenanceWindow)

-- | User-provided value that will be included in any Amazon CloudWatch
-- Events events raised while running tasks for these targets in this
-- maintenance window.
registerTargetWithMaintenanceWindow_ownerInformation :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Prelude.Maybe Prelude.Text)
registerTargetWithMaintenanceWindow_ownerInformation = Lens.lens (\RegisterTargetWithMaintenanceWindow' {ownerInformation} -> ownerInformation) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {ownerInformation = a} :: RegisterTargetWithMaintenanceWindow) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the maintenance window the target should be registered with.
registerTargetWithMaintenanceWindow_windowId :: Lens.Lens' RegisterTargetWithMaintenanceWindow Prelude.Text
registerTargetWithMaintenanceWindow_windowId = Lens.lens (\RegisterTargetWithMaintenanceWindow' {windowId} -> windowId) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {windowId = a} :: RegisterTargetWithMaintenanceWindow)

-- | The type of target being registered with the maintenance window.
registerTargetWithMaintenanceWindow_resourceType :: Lens.Lens' RegisterTargetWithMaintenanceWindow MaintenanceWindowResourceType
registerTargetWithMaintenanceWindow_resourceType = Lens.lens (\RegisterTargetWithMaintenanceWindow' {resourceType} -> resourceType) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {resourceType = a} :: RegisterTargetWithMaintenanceWindow)

-- | The targets to register with the maintenance window. In other words, the
-- managed nodes to run commands on when the maintenance window runs.
--
-- If a single maintenance window task is registered with multiple targets,
-- its task invocations occur sequentially and not in parallel. If your
-- task must run on multiple targets at the same time, register a task for
-- each target individually and assign each task the same priority level.
--
-- You can specify targets using managed node IDs, resource group names, or
-- tags that have been applied to managed nodes.
--
-- __Example 1__: Specify managed node IDs
--
-- @Key=InstanceIds,Values=\<instance-id-1>,\<instance-id-2>,\<instance-id-3>@
--
-- __Example 2__: Use tag key-pairs applied to managed nodes
--
-- @Key=tag:\<my-tag-key>,Values=\<my-tag-value-1>,\<my-tag-value-2>@
--
-- __Example 3__: Use tag-keys applied to managed nodes
--
-- @Key=tag-key,Values=\<my-tag-key-1>,\<my-tag-key-2>@
--
-- __Example 4__: Use resource group names
--
-- @Key=resource-groups:Name,Values=\<resource-group-name>@
--
-- __Example 5__: Use filters for resource group types
--
-- @Key=resource-groups:ResourceTypeFilters,Values=\<resource-type-1>,\<resource-type-2>@
--
-- For @Key=resource-groups:ResourceTypeFilters@, specify resource types in
-- the following format
--
-- @Key=resource-groups:ResourceTypeFilters,Values=AWS::EC2::INSTANCE,AWS::EC2::VPC@
--
-- For more information about these examples formats, including the best
-- use case for each one, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window>
-- in the /Amazon Web Services Systems Manager User Guide/.
registerTargetWithMaintenanceWindow_targets :: Lens.Lens' RegisterTargetWithMaintenanceWindow [Target]
registerTargetWithMaintenanceWindow_targets = Lens.lens (\RegisterTargetWithMaintenanceWindow' {targets} -> targets) (\s@RegisterTargetWithMaintenanceWindow' {} a -> s {targets = a} :: RegisterTargetWithMaintenanceWindow) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    RegisterTargetWithMaintenanceWindow
  where
  type
    AWSResponse RegisterTargetWithMaintenanceWindow =
      RegisterTargetWithMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTargetWithMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowTargetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterTargetWithMaintenanceWindow
  where
  hashWithSalt
    _salt
    RegisterTargetWithMaintenanceWindow' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` ownerInformation
        `Prelude.hashWithSalt` windowId
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` targets

instance
  Prelude.NFData
    RegisterTargetWithMaintenanceWindow
  where
  rnf RegisterTargetWithMaintenanceWindow' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerInformation
      `Prelude.seq` Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf targets

instance
  Data.ToHeaders
    RegisterTargetWithMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.RegisterTargetWithMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    RegisterTargetWithMaintenanceWindow
  where
  toJSON RegisterTargetWithMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("OwnerInformation" Data..=)
              Prelude.<$> ownerInformation,
            Prelude.Just ("WindowId" Data..= windowId),
            Prelude.Just ("ResourceType" Data..= resourceType),
            Prelude.Just ("Targets" Data..= targets)
          ]
      )

instance
  Data.ToPath
    RegisterTargetWithMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RegisterTargetWithMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterTargetWithMaintenanceWindowResponse' smart constructor.
data RegisterTargetWithMaintenanceWindowResponse = RegisterTargetWithMaintenanceWindowResponse'
  { -- | The ID of the target definition in this maintenance window.
    windowTargetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTargetWithMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTargetId', 'registerTargetWithMaintenanceWindowResponse_windowTargetId' - The ID of the target definition in this maintenance window.
--
-- 'httpStatus', 'registerTargetWithMaintenanceWindowResponse_httpStatus' - The response's http status code.
newRegisterTargetWithMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTargetWithMaintenanceWindowResponse
newRegisterTargetWithMaintenanceWindowResponse
  pHttpStatus_ =
    RegisterTargetWithMaintenanceWindowResponse'
      { windowTargetId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the target definition in this maintenance window.
registerTargetWithMaintenanceWindowResponse_windowTargetId :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
registerTargetWithMaintenanceWindowResponse_windowTargetId = Lens.lens (\RegisterTargetWithMaintenanceWindowResponse' {windowTargetId} -> windowTargetId) (\s@RegisterTargetWithMaintenanceWindowResponse' {} a -> s {windowTargetId = a} :: RegisterTargetWithMaintenanceWindowResponse)

-- | The response's http status code.
registerTargetWithMaintenanceWindowResponse_httpStatus :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse Prelude.Int
registerTargetWithMaintenanceWindowResponse_httpStatus = Lens.lens (\RegisterTargetWithMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@RegisterTargetWithMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: RegisterTargetWithMaintenanceWindowResponse)

instance
  Prelude.NFData
    RegisterTargetWithMaintenanceWindowResponse
  where
  rnf RegisterTargetWithMaintenanceWindowResponse' {..} =
    Prelude.rnf windowTargetId
      `Prelude.seq` Prelude.rnf httpStatus
