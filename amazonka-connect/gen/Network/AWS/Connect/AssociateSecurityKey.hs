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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | A valid security key in PEM format.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  AssociateSecurityKey
newAssociateSecurityKey pInstanceId_ pKey_ =
  AssociateSecurityKey'
    { instanceId = pInstanceId_,
      key = pKey_
    }

-- | The identifier of the Amazon Connect instance.
associateSecurityKey_instanceId :: Lens.Lens' AssociateSecurityKey Prelude.Text
associateSecurityKey_instanceId = Lens.lens (\AssociateSecurityKey' {instanceId} -> instanceId) (\s@AssociateSecurityKey' {} a -> s {instanceId = a} :: AssociateSecurityKey)

-- | A valid security key in PEM format.
associateSecurityKey_key :: Lens.Lens' AssociateSecurityKey Prelude.Text
associateSecurityKey_key = Lens.lens (\AssociateSecurityKey' {key} -> key) (\s@AssociateSecurityKey' {} a -> s {key = a} :: AssociateSecurityKey)

instance Prelude.AWSRequest AssociateSecurityKey where
  type
    Rs AssociateSecurityKey =
      AssociateSecurityKeyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSecurityKeyResponse'
            Prelude.<$> (x Prelude..?> "AssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSecurityKey

instance Prelude.NFData AssociateSecurityKey

instance Prelude.ToHeaders AssociateSecurityKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateSecurityKey where
  toJSON AssociateSecurityKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Key" Prelude..= key)]
      )

instance Prelude.ToPath AssociateSecurityKey where
  toPath AssociateSecurityKey' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/security-key"
      ]

instance Prelude.ToQuery AssociateSecurityKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AssociateSecurityKeyResponse
newAssociateSecurityKeyResponse pHttpStatus_ =
  AssociateSecurityKeyResponse'
    { associationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
associateSecurityKeyResponse_associationId :: Lens.Lens' AssociateSecurityKeyResponse (Prelude.Maybe Prelude.Text)
associateSecurityKeyResponse_associationId = Lens.lens (\AssociateSecurityKeyResponse' {associationId} -> associationId) (\s@AssociateSecurityKeyResponse' {} a -> s {associationId = a} :: AssociateSecurityKeyResponse)

-- | The response's http status code.
associateSecurityKeyResponse_httpStatus :: Lens.Lens' AssociateSecurityKeyResponse Prelude.Int
associateSecurityKeyResponse_httpStatus = Lens.lens (\AssociateSecurityKeyResponse' {httpStatus} -> httpStatus) (\s@AssociateSecurityKeyResponse' {} a -> s {httpStatus = a} :: AssociateSecurityKeyResponse)

instance Prelude.NFData AssociateSecurityKeyResponse
