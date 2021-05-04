{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroupVersion' smart constructor.
data CreateGroupVersion = CreateGroupVersion'
  { -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateGroupVersion
newCreateGroupVersion pGroupId_ =
  CreateGroupVersion'
    { coreDefinitionVersionArn =
        Prelude.Nothing,
      connectorDefinitionVersionArn = Prelude.Nothing,
      subscriptionDefinitionVersionArn = Prelude.Nothing,
      loggerDefinitionVersionArn = Prelude.Nothing,
      resourceDefinitionVersionArn = Prelude.Nothing,
      functionDefinitionVersionArn = Prelude.Nothing,
      amznClientToken = Prelude.Nothing,
      deviceDefinitionVersionArn = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | The ARN of the core definition version for this group.
createGroupVersion_coreDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_coreDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {coreDefinitionVersionArn} -> coreDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {coreDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the connector definition version for this group.
createGroupVersion_connectorDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_connectorDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {connectorDefinitionVersionArn} -> connectorDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {connectorDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the subscription definition version for this group.
createGroupVersion_subscriptionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_subscriptionDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {subscriptionDefinitionVersionArn} -> subscriptionDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {subscriptionDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the logger definition version for this group.
createGroupVersion_loggerDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_loggerDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {loggerDefinitionVersionArn} -> loggerDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {loggerDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the resource definition version for this group.
createGroupVersion_resourceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_resourceDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {resourceDefinitionVersionArn} -> resourceDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {resourceDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ARN of the function definition version for this group.
createGroupVersion_functionDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_functionDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {functionDefinitionVersionArn} -> functionDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {functionDefinitionVersionArn = a} :: CreateGroupVersion)

-- | A client token used to correlate requests and responses.
createGroupVersion_amznClientToken :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_amznClientToken = Lens.lens (\CreateGroupVersion' {amznClientToken} -> amznClientToken) (\s@CreateGroupVersion' {} a -> s {amznClientToken = a} :: CreateGroupVersion)

-- | The ARN of the device definition version for this group.
createGroupVersion_deviceDefinitionVersionArn :: Lens.Lens' CreateGroupVersion (Prelude.Maybe Prelude.Text)
createGroupVersion_deviceDefinitionVersionArn = Lens.lens (\CreateGroupVersion' {deviceDefinitionVersionArn} -> deviceDefinitionVersionArn) (\s@CreateGroupVersion' {} a -> s {deviceDefinitionVersionArn = a} :: CreateGroupVersion)

-- | The ID of the Greengrass group.
createGroupVersion_groupId :: Lens.Lens' CreateGroupVersion Prelude.Text
createGroupVersion_groupId = Lens.lens (\CreateGroupVersion' {groupId} -> groupId) (\s@CreateGroupVersion' {} a -> s {groupId = a} :: CreateGroupVersion)

instance Prelude.AWSRequest CreateGroupVersion where
  type
    Rs CreateGroupVersion =
      CreateGroupVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupVersionResponse'
            Prelude.<$> (x Prelude..?> "CreationTimestamp")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroupVersion

instance Prelude.NFData CreateGroupVersion

instance Prelude.ToHeaders CreateGroupVersion where
  toHeaders CreateGroupVersion' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateGroupVersion where
  toJSON CreateGroupVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CoreDefinitionVersionArn" Prelude..=)
              Prelude.<$> coreDefinitionVersionArn,
            ("ConnectorDefinitionVersionArn" Prelude..=)
              Prelude.<$> connectorDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Prelude..=)
              Prelude.<$> subscriptionDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Prelude..=)
              Prelude.<$> loggerDefinitionVersionArn,
            ("ResourceDefinitionVersionArn" Prelude..=)
              Prelude.<$> resourceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Prelude..=)
              Prelude.<$> functionDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Prelude..=)
              Prelude.<$> deviceDefinitionVersionArn
          ]
      )

instance Prelude.ToPath CreateGroupVersion where
  toPath CreateGroupVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/versions"
      ]

instance Prelude.ToQuery CreateGroupVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupVersionResponse' smart constructor.
data CreateGroupVersionResponse = CreateGroupVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateGroupVersionResponse
newCreateGroupVersionResponse pHttpStatus_ =
  CreateGroupVersionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the version was created.
createGroupVersionResponse_creationTimestamp :: Lens.Lens' CreateGroupVersionResponse (Prelude.Maybe Prelude.Text)
createGroupVersionResponse_creationTimestamp = Lens.lens (\CreateGroupVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@CreateGroupVersionResponse' {} a -> s {creationTimestamp = a} :: CreateGroupVersionResponse)

-- | The ARN of the version.
createGroupVersionResponse_arn :: Lens.Lens' CreateGroupVersionResponse (Prelude.Maybe Prelude.Text)
createGroupVersionResponse_arn = Lens.lens (\CreateGroupVersionResponse' {arn} -> arn) (\s@CreateGroupVersionResponse' {} a -> s {arn = a} :: CreateGroupVersionResponse)

-- | The ID of the parent definition that the version is associated with.
createGroupVersionResponse_id :: Lens.Lens' CreateGroupVersionResponse (Prelude.Maybe Prelude.Text)
createGroupVersionResponse_id = Lens.lens (\CreateGroupVersionResponse' {id} -> id) (\s@CreateGroupVersionResponse' {} a -> s {id = a} :: CreateGroupVersionResponse)

-- | The ID of the version.
createGroupVersionResponse_version :: Lens.Lens' CreateGroupVersionResponse (Prelude.Maybe Prelude.Text)
createGroupVersionResponse_version = Lens.lens (\CreateGroupVersionResponse' {version} -> version) (\s@CreateGroupVersionResponse' {} a -> s {version = a} :: CreateGroupVersionResponse)

-- | The response's http status code.
createGroupVersionResponse_httpStatus :: Lens.Lens' CreateGroupVersionResponse Prelude.Int
createGroupVersionResponse_httpStatus = Lens.lens (\CreateGroupVersionResponse' {httpStatus} -> httpStatus) (\s@CreateGroupVersionResponse' {} a -> s {httpStatus = a} :: CreateGroupVersionResponse)

instance Prelude.NFData CreateGroupVersionResponse
