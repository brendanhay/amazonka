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
-- Module      : Network.AWS.Greengrass.CreateGroupVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a group which has already been defined.
module Network.AWS.Greengrass.CreateGroupVersion
  ( -- * Creating a Request
    CreateGroupVersion (..),
    newCreateGroupVersion,

    -- * Request Lenses
    createGroupVersion_coreDefinitionVersionArn,
    createGroupVersion_connectorDefinitionVersionArn,
    createGroupVersion_subscriptionDefinitionVersionArn,
    createGroupVersion_loggerDefinitionVersionArn,
    createGroupVersion_resourceDefinitionVersionArn,
    createGroupVersion_functionDefinitionVersionArn,
    createGroupVersion_amznClientToken,
    createGroupVersion_deviceDefinitionVersionArn,
    createGroupVersion_groupId,

    -- * Destructuring the Response
    CreateGroupVersionResponse (..),
    newCreateGroupVersionResponse,

    -- * Response Lenses
    createGroupVersionResponse_creationTimestamp,
    createGroupVersionResponse_arn,
    createGroupVersionResponse_id,
    createGroupVersionResponse_version,
    createGroupVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionVersionArn', 'createGroupVersion_coreDefinitionVersionArn' - The ARN of the core definition version for this group.
--
-- 'connectorDefinitionVersionArn', 'createGroupVersion_connectorDefinitionVersionArn' - The ARN of the connector definition version for this group.
--
-- 'subscriptionDefinitionVersionArn', 'createGroupVersion_subscriptionDefinitionVersionArn' - The ARN of the subscription definition version for this group.
--
-- 'loggerDefinitionVersionArn', 'createGroupVersion_loggerDefinitionVersionArn' - The ARN of the logger definition version for this group.
--
-- 'resourceDefinitionVersionArn', 'createGroupVersion_resourceDefinitionVersionArn' - The ARN of the resource definition version for this group.
--
-- 'functionDefinitionVersionArn', 'createGroupVersion_functionDefinitionVersionArn' - The ARN of the function definition version for this group.
--
-- 'amznClientToken', 'createGroupVersion_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'deviceDefinitionVersionArn', 'createGroupVersion_deviceDefinitionVersionArn' - The ARN of the device definition version for this group.
--
-- 'groupId', 'createGroupVersion_groupId' - The ID of the Greengrass group.
newCreateGroupVersion ::
  -- | 'groupId'
  Core.Text ->
  CreateGroupVersion
newCreateGroupVersion pGroupId_ =
  CreateGroupVersion'
    { coreDefinitionVersionArn =
        Core.Nothing,
      connectorDefinitionVersionArn = Core.Nothing,
      subscriptionDefinitionVersionArn = Core.Nothing,
      loggerDefinitionVersionArn = Core.Nothing,
      resourceDefinitionVersionArn = Core.Nothing,
      functionDefinitionVersionArn = Core.Nothing,
      amznClientToken = Core.Nothing,
      deviceDefinitionVersionArn = Core.Nothing,
      groupId = pGroupId_
    }

-- | The ARN of the core definition version for this group.
createGroupVersion_coreDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_coreDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {coreDefinitionVersionArn} -> coreDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {coreDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the connector definition version for this group.
createGroupVersion_connectorDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_connectorDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {connectorDefinitionVersionArn} -> connectorDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {connectorDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the subscription definition version for this group.
createGroupVersion_subscriptionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_subscriptionDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {subscriptionDefinitionVersionArn} -> subscriptionDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {subscriptionDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the logger definition version for this group.
createGroupVersion_loggerDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_loggerDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {loggerDefinitionVersionArn} -> loggerDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {loggerDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the resource definition version for this group.
createGroupVersion_resourceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_resourceDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {resourceDefinitionVersionArn} -> resourceDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {resourceDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the function definition version for this group.
createGroupVersion_functionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_functionDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {functionDefinitionVersionArn} -> functionDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {functionDefinitionVersionArn = a} :: CreateGroupVersion)

-- | A client token used to correlate requests and responses.
createGroupVersion_amznClientToken :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_amznClientToken = Lens.lens (\CreateGroupVersion' {amznClientToken} -> amznClientToken) (\s@CreateGroupVersion' {} a -> s {amznClientToken = a} :: CreateGroupVersion)

-- | The ARN of the device definition version for this group.
createGroupVersion_deviceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Core.Maybe Core.Text)
createGroupVersion_deviceDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {deviceDefinitionVersionArn} -> deviceDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {deviceDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ID of the Greengrass group.
createGroupVersion_groupId :: Lens.Lens' CreateGroupVersion Core.Text
createGroupVersion_groupId = Lens.lens (\CreateGroupVersion' {groupId} -> groupId) (\s@CreateGroupVersion' {} a -> s {groupId = a} :: CreateGroupVersion)

instance Core.AWSRequest CreateGroupVersion where
  type
    AWSResponse CreateGroupVersion =
      CreateGroupVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupVersionResponse'
            Core.<$> (x Core..?> "CreationTimestamp")
            Core.<*> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (x Core..?> "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGroupVersion

instance Core.NFData CreateGroupVersion

instance Core.ToHeaders CreateGroupVersion where
  toHeaders CreateGroupVersion' {..} =
    Core.mconcat
      [ "X-Amzn-Client-Token" Core.=# amznClientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON CreateGroupVersion where
  toJSON CreateGroupVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CoreDefinitionVersionArn" Core..=)
              Core.<$> coreDefinitionVersionArn,
            ("ConnectorDefinitionVersionArn" Core..=)
              Core.<$> connectorDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Core..=)
              Core.<$> subscriptionDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Core..=)
              Core.<$> loggerDefinitionVersionArn,
            ("ResourceDefinitionVersionArn" Core..=)
              Core.<$> resourceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Core..=)
              Core.<$> functionDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Core..=)
              Core.<$> deviceDefinitionVersionArn
          ]
      )

instance Core.ToPath CreateGroupVersion where
  toPath CreateGroupVersion' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/versions"
      ]

instance Core.ToQuery CreateGroupVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroupVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'createGroupVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'createGroupVersionResponse_arn' - The ARN of the version.
--
-- 'id', 'createGroupVersionResponse_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'createGroupVersionResponse_version' - The ID of the version.
--
-- 'httpStatus', 'createGroupVersionResponse_httpStatus' - The response's http status code.
newCreateGroupVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGroupVersionResponse
newCreateGroupVersionResponse pHttpStatus_ =
  CreateGroupVersionResponse'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createGroupVersionResponse_creationTimestamp :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
createGroupVersionResponse_creationTimestamp = Lens.lens (\CreateGroupVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateGroupVersionResponse' {} a -> s {creationTimestamp = a} :: CreateGroupVersionResponse)

-- | The ARN of the version.
createGroupVersionResponse_arn :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
createGroupVersionResponse_arn = Lens.lens (\CreateGroupVersionResponse' {arn} -> arn) (\s@CreateGroupVersionResponse' {} a -> s {arn = a} :: CreateGroupVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createGroupVersionResponse_id :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
createGroupVersionResponse_id = Lens.lens (\CreateGroupVersionResponse' {id} -> id) (\s@CreateGroupVersionResponse' {} a -> s {id = a} :: CreateGroupVersionResponse)

-- | The ID of the version.
createGroupVersionResponse_version :: Lens.Lens' CreateGroupVersionResponse (Core.Maybe Core.Text)
createGroupVersionResponse_version = Lens.lens (\CreateGroupVersionResponse' {version} -> version) (\s@CreateGroupVersionResponse' {} a -> s {version = a} :: CreateGroupVersionResponse)

-- | The response's http status code.
createGroupVersionResponse_httpStatus :: Lens.Lens' CreateGroupVersionResponse Core.Int
createGroupVersionResponse_httpStatus = Lens.lens (\CreateGroupVersionResponse' {httpStatus} -> httpStatus) (\s@CreateGroupVersionResponse' {} a -> s {httpStatus = a} :: CreateGroupVersionResponse)

instance Core.NFData CreateGroupVersionResponse
