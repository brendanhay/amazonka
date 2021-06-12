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
-- Module      : Network.AWS.Connect.CreateInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Initiates an Amazon Connect instance with all the supported channels
-- enabled. It does not attach any storage, such as Amazon Simple Storage
-- Service (Amazon S3) or Amazon Kinesis. It also does not allow for any
-- configurations on features, such as Contact Lens for Amazon Connect.
module Network.AWS.Connect.CreateInstance
  ( -- * Creating a Request
    CreateInstance (..),
    newCreateInstance,

    -- * Request Lenses
    createInstance_instanceAlias,
    createInstance_directoryId,
    createInstance_clientToken,
    createInstance_identityManagementType,
    createInstance_inboundCallsEnabled,
    createInstance_outboundCallsEnabled,

    -- * Destructuring the Response
    CreateInstanceResponse (..),
    newCreateInstanceResponse,

    -- * Response Lenses
    createInstanceResponse_arn,
    createInstanceResponse_id,
    createInstanceResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | The name for your instance.
    instanceAlias :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The identifier for the directory.
    directoryId :: Core.Maybe Core.Text,
    -- | The idempotency token.
    clientToken :: Core.Maybe Core.Text,
    -- | The type of identity management for your Amazon Connect users.
    identityManagementType :: DirectoryType,
    -- | Your contact center handles incoming contacts.
    inboundCallsEnabled :: Core.Bool,
    -- | Your contact center allows outbound calls.
    outboundCallsEnabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceAlias', 'createInstance_instanceAlias' - The name for your instance.
--
-- 'directoryId', 'createInstance_directoryId' - The identifier for the directory.
--
-- 'clientToken', 'createInstance_clientToken' - The idempotency token.
--
-- 'identityManagementType', 'createInstance_identityManagementType' - The type of identity management for your Amazon Connect users.
--
-- 'inboundCallsEnabled', 'createInstance_inboundCallsEnabled' - Your contact center handles incoming contacts.
--
-- 'outboundCallsEnabled', 'createInstance_outboundCallsEnabled' - Your contact center allows outbound calls.
newCreateInstance ::
  -- | 'identityManagementType'
  DirectoryType ->
  -- | 'inboundCallsEnabled'
  Core.Bool ->
  -- | 'outboundCallsEnabled'
  Core.Bool ->
  CreateInstance
newCreateInstance
  pIdentityManagementType_
  pInboundCallsEnabled_
  pOutboundCallsEnabled_ =
    CreateInstance'
      { instanceAlias = Core.Nothing,
        directoryId = Core.Nothing,
        clientToken = Core.Nothing,
        identityManagementType = pIdentityManagementType_,
        inboundCallsEnabled = pInboundCallsEnabled_,
        outboundCallsEnabled = pOutboundCallsEnabled_
      }

-- | The name for your instance.
createInstance_instanceAlias :: Lens.Lens' CreateInstance (Core.Maybe Core.Text)
createInstance_instanceAlias = Lens.lens (\CreateInstance' {instanceAlias} -> instanceAlias) (\s@CreateInstance' {} a -> s {instanceAlias = a} :: CreateInstance) Core.. Lens.mapping Core._Sensitive

-- | The identifier for the directory.
createInstance_directoryId :: Lens.Lens' CreateInstance (Core.Maybe Core.Text)
createInstance_directoryId = Lens.lens (\CreateInstance' {directoryId} -> directoryId) (\s@CreateInstance' {} a -> s {directoryId = a} :: CreateInstance)

-- | The idempotency token.
createInstance_clientToken :: Lens.Lens' CreateInstance (Core.Maybe Core.Text)
createInstance_clientToken = Lens.lens (\CreateInstance' {clientToken} -> clientToken) (\s@CreateInstance' {} a -> s {clientToken = a} :: CreateInstance)

-- | The type of identity management for your Amazon Connect users.
createInstance_identityManagementType :: Lens.Lens' CreateInstance DirectoryType
createInstance_identityManagementType = Lens.lens (\CreateInstance' {identityManagementType} -> identityManagementType) (\s@CreateInstance' {} a -> s {identityManagementType = a} :: CreateInstance)

-- | Your contact center handles incoming contacts.
createInstance_inboundCallsEnabled :: Lens.Lens' CreateInstance Core.Bool
createInstance_inboundCallsEnabled = Lens.lens (\CreateInstance' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@CreateInstance' {} a -> s {inboundCallsEnabled = a} :: CreateInstance)

-- | Your contact center allows outbound calls.
createInstance_outboundCallsEnabled :: Lens.Lens' CreateInstance Core.Bool
createInstance_outboundCallsEnabled = Lens.lens (\CreateInstance' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@CreateInstance' {} a -> s {outboundCallsEnabled = a} :: CreateInstance)

instance Core.AWSRequest CreateInstance where
  type
    AWSResponse CreateInstance =
      CreateInstanceResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Core.<$> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Id")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInstance

instance Core.NFData CreateInstance

instance Core.ToHeaders CreateInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceAlias" Core..=) Core.<$> instanceAlias,
            ("DirectoryId" Core..=) Core.<$> directoryId,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just
              ( "IdentityManagementType"
                  Core..= identityManagementType
              ),
            Core.Just
              ("InboundCallsEnabled" Core..= inboundCallsEnabled),
            Core.Just
              ( "OutboundCallsEnabled"
                  Core..= outboundCallsEnabled
              )
          ]
      )

instance Core.ToPath CreateInstance where
  toPath = Core.const "/instance"

instance Core.ToQuery CreateInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Core.Maybe Core.Text,
    -- | The identifier for the instance.
    id :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createInstanceResponse_arn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'id', 'createInstanceResponse_id' - The identifier for the instance.
--
-- 'httpStatus', 'createInstanceResponse_httpStatus' - The response's http status code.
newCreateInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateInstanceResponse
newCreateInstanceResponse pHttpStatus_ =
  CreateInstanceResponse'
    { arn = Core.Nothing,
      id = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the instance.
createInstanceResponse_arn :: Lens.Lens' CreateInstanceResponse (Core.Maybe Core.Text)
createInstanceResponse_arn = Lens.lens (\CreateInstanceResponse' {arn} -> arn) (\s@CreateInstanceResponse' {} a -> s {arn = a} :: CreateInstanceResponse)

-- | The identifier for the instance.
createInstanceResponse_id :: Lens.Lens' CreateInstanceResponse (Core.Maybe Core.Text)
createInstanceResponse_id = Lens.lens (\CreateInstanceResponse' {id} -> id) (\s@CreateInstanceResponse' {} a -> s {id = a} :: CreateInstanceResponse)

-- | The response's http status code.
createInstanceResponse_httpStatus :: Lens.Lens' CreateInstanceResponse Core.Int
createInstanceResponse_httpStatus = Lens.lens (\CreateInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceResponse' {} a -> s {httpStatus = a} :: CreateInstanceResponse)

instance Core.NFData CreateInstanceResponse
