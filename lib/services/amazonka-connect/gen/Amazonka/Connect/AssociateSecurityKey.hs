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
-- Module      : Amazonka.Connect.AssociateSecurityKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates a security key to the instance.
module Amazonka.Connect.AssociateSecurityKey
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSecurityKey' smart constructor.
data AssociateSecurityKey = AssociateSecurityKey'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | A valid security key in PEM format.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSecurityKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateSecurityKey_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
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

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
associateSecurityKey_instanceId :: Lens.Lens' AssociateSecurityKey Prelude.Text
associateSecurityKey_instanceId = Lens.lens (\AssociateSecurityKey' {instanceId} -> instanceId) (\s@AssociateSecurityKey' {} a -> s {instanceId = a} :: AssociateSecurityKey)

-- | A valid security key in PEM format.
associateSecurityKey_key :: Lens.Lens' AssociateSecurityKey Prelude.Text
associateSecurityKey_key = Lens.lens (\AssociateSecurityKey' {key} -> key) (\s@AssociateSecurityKey' {} a -> s {key = a} :: AssociateSecurityKey)

instance Core.AWSRequest AssociateSecurityKey where
  type
    AWSResponse AssociateSecurityKey =
      AssociateSecurityKeyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSecurityKeyResponse'
            Prelude.<$> (x Data..?> "AssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSecurityKey where
  hashWithSalt _salt AssociateSecurityKey' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` key

instance Prelude.NFData AssociateSecurityKey where
  rnf AssociateSecurityKey' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders AssociateSecurityKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSecurityKey where
  toJSON AssociateSecurityKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Key" Data..= key)]
      )

instance Data.ToPath AssociateSecurityKey where
  toPath AssociateSecurityKey' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId, "/security-key"]

instance Data.ToQuery AssociateSecurityKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSecurityKeyResponse' smart constructor.
data AssociateSecurityKeyResponse = AssociateSecurityKeyResponse'
  { -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AssociateSecurityKeyResponse where
  rnf AssociateSecurityKeyResponse' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf httpStatus
