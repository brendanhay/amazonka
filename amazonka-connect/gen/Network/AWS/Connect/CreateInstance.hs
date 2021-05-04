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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInstance' smart constructor.
data CreateInstance = CreateInstance'
  { -- | The name for your instance.
    instanceAlias :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The identifier for the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The type of identity management for your Amazon Connect users.
    identityManagementType :: DirectoryType,
    -- | Your contact center handles incoming contacts.
    inboundCallsEnabled :: Prelude.Bool,
    -- | Your contact center allows outbound calls.
    outboundCallsEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Bool ->
  -- | 'outboundCallsEnabled'
  Prelude.Bool ->
  CreateInstance
newCreateInstance
  pIdentityManagementType_
  pInboundCallsEnabled_
  pOutboundCallsEnabled_ =
    CreateInstance'
      { instanceAlias = Prelude.Nothing,
        directoryId = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        identityManagementType = pIdentityManagementType_,
        inboundCallsEnabled = pInboundCallsEnabled_,
        outboundCallsEnabled = pOutboundCallsEnabled_
      }

-- | The name for your instance.
createInstance_instanceAlias :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_instanceAlias = Lens.lens (\CreateInstance' {instanceAlias} -> instanceAlias) (\s@CreateInstance' {} a -> s {instanceAlias = a} :: CreateInstance) Prelude.. Lens.mapping Prelude._Sensitive

-- | The identifier for the directory.
createInstance_directoryId :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_directoryId = Lens.lens (\CreateInstance' {directoryId} -> directoryId) (\s@CreateInstance' {} a -> s {directoryId = a} :: CreateInstance)

-- | The idempotency token.
createInstance_clientToken :: Lens.Lens' CreateInstance (Prelude.Maybe Prelude.Text)
createInstance_clientToken = Lens.lens (\CreateInstance' {clientToken} -> clientToken) (\s@CreateInstance' {} a -> s {clientToken = a} :: CreateInstance)

-- | The type of identity management for your Amazon Connect users.
createInstance_identityManagementType :: Lens.Lens' CreateInstance DirectoryType
createInstance_identityManagementType = Lens.lens (\CreateInstance' {identityManagementType} -> identityManagementType) (\s@CreateInstance' {} a -> s {identityManagementType = a} :: CreateInstance)

-- | Your contact center handles incoming contacts.
createInstance_inboundCallsEnabled :: Lens.Lens' CreateInstance Prelude.Bool
createInstance_inboundCallsEnabled = Lens.lens (\CreateInstance' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@CreateInstance' {} a -> s {inboundCallsEnabled = a} :: CreateInstance)

-- | Your contact center allows outbound calls.
createInstance_outboundCallsEnabled :: Lens.Lens' CreateInstance Prelude.Bool
createInstance_outboundCallsEnabled = Lens.lens (\CreateInstance' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@CreateInstance' {} a -> s {outboundCallsEnabled = a} :: CreateInstance)

instance Prelude.AWSRequest CreateInstance where
  type Rs CreateInstance = CreateInstanceResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceResponse'
            Prelude.<$> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstance

instance Prelude.NFData CreateInstance

instance Prelude.ToHeaders CreateInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateInstance where
  toJSON CreateInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceAlias" Prelude..=)
              Prelude.<$> instanceAlias,
            ("DirectoryId" Prelude..=) Prelude.<$> directoryId,
            ("ClientToken" Prelude..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "IdentityManagementType"
                  Prelude..= identityManagementType
              ),
            Prelude.Just
              ( "InboundCallsEnabled"
                  Prelude..= inboundCallsEnabled
              ),
            Prelude.Just
              ( "OutboundCallsEnabled"
                  Prelude..= outboundCallsEnabled
              )
          ]
      )

instance Prelude.ToPath CreateInstance where
  toPath = Prelude.const "/instance"

instance Prelude.ToQuery CreateInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstanceResponse' smart constructor.
data CreateInstanceResponse = CreateInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateInstanceResponse
newCreateInstanceResponse pHttpStatus_ =
  CreateInstanceResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the instance.
createInstanceResponse_arn :: Lens.Lens' CreateInstanceResponse (Prelude.Maybe Prelude.Text)
createInstanceResponse_arn = Lens.lens (\CreateInstanceResponse' {arn} -> arn) (\s@CreateInstanceResponse' {} a -> s {arn = a} :: CreateInstanceResponse)

-- | The identifier for the instance.
createInstanceResponse_id :: Lens.Lens' CreateInstanceResponse (Prelude.Maybe Prelude.Text)
createInstanceResponse_id = Lens.lens (\CreateInstanceResponse' {id} -> id) (\s@CreateInstanceResponse' {} a -> s {id = a} :: CreateInstanceResponse)

-- | The response's http status code.
createInstanceResponse_httpStatus :: Lens.Lens' CreateInstanceResponse Prelude.Int
createInstanceResponse_httpStatus = Lens.lens (\CreateInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceResponse' {} a -> s {httpStatus = a} :: CreateInstanceResponse)

instance Prelude.NFData CreateInstanceResponse
