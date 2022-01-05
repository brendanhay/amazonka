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
-- Module      : Amazonka.Lightsail.DeleteKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific SSH key pair.
--
-- The @delete key pair@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @key pair name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteKeyPair
  ( -- * Creating a Request
    DeleteKeyPair (..),
    newDeleteKeyPair,

    -- * Request Lenses
    deleteKeyPair_keyPairName,

    -- * Destructuring the Response
    DeleteKeyPairResponse (..),
    newDeleteKeyPairResponse,

    -- * Response Lenses
    deleteKeyPairResponse_operation,
    deleteKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { -- | The name of the key pair to delete.
    keyPairName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairName', 'deleteKeyPair_keyPairName' - The name of the key pair to delete.
newDeleteKeyPair ::
  -- | 'keyPairName'
  Prelude.Text ->
  DeleteKeyPair
newDeleteKeyPair pKeyPairName_ =
  DeleteKeyPair' {keyPairName = pKeyPairName_}

-- | The name of the key pair to delete.
deleteKeyPair_keyPairName :: Lens.Lens' DeleteKeyPair Prelude.Text
deleteKeyPair_keyPairName = Lens.lens (\DeleteKeyPair' {keyPairName} -> keyPairName) (\s@DeleteKeyPair' {} a -> s {keyPairName = a} :: DeleteKeyPair)

instance Core.AWSRequest DeleteKeyPair where
  type
    AWSResponse DeleteKeyPair =
      DeleteKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKeyPairResponse'
            Prelude.<$> (x Core..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKeyPair where
  hashWithSalt _salt DeleteKeyPair' {..} =
    _salt `Prelude.hashWithSalt` keyPairName

instance Prelude.NFData DeleteKeyPair where
  rnf DeleteKeyPair' {..} = Prelude.rnf keyPairName

instance Core.ToHeaders DeleteKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteKeyPair where
  toJSON DeleteKeyPair' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("keyPairName" Core..= keyPairName)]
      )

instance Core.ToPath DeleteKeyPair where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'deleteKeyPairResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteKeyPairResponse_httpStatus' - The response's http status code.
newDeleteKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKeyPairResponse
newDeleteKeyPairResponse pHttpStatus_ =
  DeleteKeyPairResponse'
    { operation = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteKeyPairResponse_operation :: Lens.Lens' DeleteKeyPairResponse (Prelude.Maybe Operation)
deleteKeyPairResponse_operation = Lens.lens (\DeleteKeyPairResponse' {operation} -> operation) (\s@DeleteKeyPairResponse' {} a -> s {operation = a} :: DeleteKeyPairResponse)

-- | The response's http status code.
deleteKeyPairResponse_httpStatus :: Lens.Lens' DeleteKeyPairResponse Prelude.Int
deleteKeyPairResponse_httpStatus = Lens.lens (\DeleteKeyPairResponse' {httpStatus} -> httpStatus) (\s@DeleteKeyPairResponse' {} a -> s {httpStatus = a} :: DeleteKeyPairResponse)

instance Prelude.NFData DeleteKeyPairResponse where
  rnf DeleteKeyPairResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
