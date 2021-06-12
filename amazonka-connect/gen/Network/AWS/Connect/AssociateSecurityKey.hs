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
-- Module      : Network.AWS.Connect.AssociateSecurityKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates a security key to the instance.
module Network.AWS.Connect.AssociateSecurityKey
  ( -- * Creating a Request
    AssociateSecurityKey (..),
    newAssociateSecurityKey,

    -- * Request Lenses
    associateSecurityKey_instanceId,
    associateSecurityKey_key,

    -- * Destructuring the Response
    AssociateSecurityKeyResponse (..),
    newAssociateSecurityKeyResponse,

    -- * Response Lenses
    associateSecurityKeyResponse_associationId,
    associateSecurityKeyResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | A valid security key in PEM format.
    key :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSecurityKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateSecurityKey_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'key', 'associateSecurityKey_key' - A valid security key in PEM format.
newAssociateSecurityKey ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'key'
  Core.Text ->
  AssociateSecurityKey
newAssociateSecurityKey pInstanceId_ pKey_ =
  AssociateSecurityKey'
    { instanceId = pInstanceId_,
      key = pKey_
    }

-- | The identifier of the Amazon Connect instance.
associateSecurityKey_instanceId :: Lens.Lens' AssociateSecurityKey Core.Text
associateSecurityKey_instanceId = Lens.lens (\AssociateSecurityKey' {instanceId} -> instanceId) (\s@AssociateSecurityKey' {} a -> s {instanceId = a} :: AssociateSecurityKey)

-- | A valid security key in PEM format.
associateSecurityKey_key :: Lens.Lens' AssociateSecurityKey Core.Text
associateSecurityKey_key = Lens.lens (\AssociateSecurityKey' {key} -> key) (\s@AssociateSecurityKey' {} a -> s {key = a} :: AssociateSecurityKey)

instance Core.AWSRequest AssociateSecurityKey where
  type
    AWSResponse AssociateSecurityKey =
      AssociateSecurityKeyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSecurityKeyResponse'
            Core.<$> (x Core..?> "AssociationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateSecurityKey

instance Core.NFData AssociateSecurityKey

instance Core.ToHeaders AssociateSecurityKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateSecurityKey where
  toJSON AssociateSecurityKey' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Key" Core..= key)])

instance Core.ToPath AssociateSecurityKey where
  toPath AssociateSecurityKey' {..} =
    Core.mconcat
      ["/instance/", Core.toBS instanceId, "/security-key"]

instance Core.ToQuery AssociateSecurityKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSecurityKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'associateSecurityKeyResponse_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'httpStatus', 'associateSecurityKeyResponse_httpStatus' - The response's http status code.
newAssociateSecurityKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateSecurityKeyResponse
newAssociateSecurityKeyResponse pHttpStatus_ =
  AssociateSecurityKeyResponse'
    { associationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
associateSecurityKeyResponse_associationId :: Lens.Lens' AssociateSecurityKeyResponse (Core.Maybe Core.Text)
associateSecurityKeyResponse_associationId = Lens.lens (\AssociateSecurityKeyResponse' {associationId} -> associationId) (\s@AssociateSecurityKeyResponse' {} a -> s {associationId = a} :: AssociateSecurityKeyResponse)

-- | The response's http status code.
associateSecurityKeyResponse_httpStatus :: Lens.Lens' AssociateSecurityKeyResponse Core.Int
associateSecurityKeyResponse_httpStatus = Lens.lens (\AssociateSecurityKeyResponse' {httpStatus} -> httpStatus) (\s@AssociateSecurityKeyResponse' {} a -> s {httpStatus = a} :: AssociateSecurityKeyResponse)

instance Core.NFData AssociateSecurityKeyResponse
