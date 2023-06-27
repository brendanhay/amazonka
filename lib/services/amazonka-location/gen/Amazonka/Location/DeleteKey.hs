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
-- Module      : Amazonka.Location.DeleteKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API key. The API key must have been deactivated
-- more than 90 days previously.
module Amazonka.Location.DeleteKey
  ( -- * Creating a Request
    DeleteKey (..),
    newDeleteKey,

    -- * Request Lenses
    deleteKey_keyName,

    -- * Destructuring the Response
    DeleteKeyResponse (..),
    newDeleteKeyResponse,

    -- * Response Lenses
    deleteKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKey' smart constructor.
data DeleteKey = DeleteKey'
  { -- | The name of the API key to delete.
    keyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'deleteKey_keyName' - The name of the API key to delete.
newDeleteKey ::
  -- | 'keyName'
  Prelude.Text ->
  DeleteKey
newDeleteKey pKeyName_ =
  DeleteKey' {keyName = pKeyName_}

-- | The name of the API key to delete.
deleteKey_keyName :: Lens.Lens' DeleteKey Prelude.Text
deleteKey_keyName = Lens.lens (\DeleteKey' {keyName} -> keyName) (\s@DeleteKey' {} a -> s {keyName = a} :: DeleteKey)

instance Core.AWSRequest DeleteKey where
  type AWSResponse DeleteKey = DeleteKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKey where
  hashWithSalt _salt DeleteKey' {..} =
    _salt `Prelude.hashWithSalt` keyName

instance Prelude.NFData DeleteKey where
  rnf DeleteKey' {..} = Prelude.rnf keyName

instance Data.ToHeaders DeleteKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteKey where
  toPath DeleteKey' {..} =
    Prelude.mconcat
      ["/metadata/v0/keys/", Data.toBS keyName]

instance Data.ToQuery DeleteKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeyResponse' smart constructor.
data DeleteKeyResponse = DeleteKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKeyResponse_httpStatus' - The response's http status code.
newDeleteKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKeyResponse
newDeleteKeyResponse pHttpStatus_ =
  DeleteKeyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteKeyResponse_httpStatus :: Lens.Lens' DeleteKeyResponse Prelude.Int
deleteKeyResponse_httpStatus = Lens.lens (\DeleteKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteKeyResponse' {} a -> s {httpStatus = a} :: DeleteKeyResponse)

instance Prelude.NFData DeleteKeyResponse where
  rnf DeleteKeyResponse' {..} = Prelude.rnf httpStatus
