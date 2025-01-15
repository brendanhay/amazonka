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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair by removing the public key from Amazon
-- Lightsail.
--
-- You can delete key pairs that were created using the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_ImportKeyPair.html ImportKeyPair>
-- and
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateKeyPair.html CreateKeyPair>
-- actions, as well as the Lightsail default key pair. A new default key
-- pair will not be created unless you launch an instance without
-- specifying a custom key pair, or you call the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_DownloadDefaultKeyPair.html DownloadDefaultKeyPair>
-- API.
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
    deleteKeyPair_expectedFingerprint,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyPair' smart constructor.
data DeleteKeyPair = DeleteKeyPair'
  { -- | The RSA fingerprint of the Lightsail default key pair to delete.
    --
    -- The @expectedFingerprint@ parameter is required only when specifying to
    -- delete a Lightsail default key pair.
    expectedFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The name of the key pair to delete.
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
-- 'expectedFingerprint', 'deleteKeyPair_expectedFingerprint' - The RSA fingerprint of the Lightsail default key pair to delete.
--
-- The @expectedFingerprint@ parameter is required only when specifying to
-- delete a Lightsail default key pair.
--
-- 'keyPairName', 'deleteKeyPair_keyPairName' - The name of the key pair to delete.
newDeleteKeyPair ::
  -- | 'keyPairName'
  Prelude.Text ->
  DeleteKeyPair
newDeleteKeyPair pKeyPairName_ =
  DeleteKeyPair'
    { expectedFingerprint =
        Prelude.Nothing,
      keyPairName = pKeyPairName_
    }

-- | The RSA fingerprint of the Lightsail default key pair to delete.
--
-- The @expectedFingerprint@ parameter is required only when specifying to
-- delete a Lightsail default key pair.
deleteKeyPair_expectedFingerprint :: Lens.Lens' DeleteKeyPair (Prelude.Maybe Prelude.Text)
deleteKeyPair_expectedFingerprint = Lens.lens (\DeleteKeyPair' {expectedFingerprint} -> expectedFingerprint) (\s@DeleteKeyPair' {} a -> s {expectedFingerprint = a} :: DeleteKeyPair)

-- | The name of the key pair to delete.
deleteKeyPair_keyPairName :: Lens.Lens' DeleteKeyPair Prelude.Text
deleteKeyPair_keyPairName = Lens.lens (\DeleteKeyPair' {keyPairName} -> keyPairName) (\s@DeleteKeyPair' {} a -> s {keyPairName = a} :: DeleteKeyPair)

instance Core.AWSRequest DeleteKeyPair where
  type
    AWSResponse DeleteKeyPair =
      DeleteKeyPairResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKeyPairResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKeyPair where
  hashWithSalt _salt DeleteKeyPair' {..} =
    _salt
      `Prelude.hashWithSalt` expectedFingerprint
      `Prelude.hashWithSalt` keyPairName

instance Prelude.NFData DeleteKeyPair where
  rnf DeleteKeyPair' {..} =
    Prelude.rnf expectedFingerprint `Prelude.seq`
      Prelude.rnf keyPairName

instance Data.ToHeaders DeleteKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteKeyPair where
  toJSON DeleteKeyPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expectedFingerprint" Data..=)
              Prelude.<$> expectedFingerprint,
            Prelude.Just ("keyPairName" Data..= keyPairName)
          ]
      )

instance Data.ToPath DeleteKeyPair where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteKeyPair where
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
    Prelude.rnf operation `Prelude.seq`
      Prelude.rnf httpStatus
