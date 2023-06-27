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
-- Module      : Amazonka.PaymentCryptography.DeleteAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the alias, but doesn\'t affect the underlying key.
--
-- Each key can have multiple aliases. To get the aliases of all keys, use
-- the ListAliases operation. To change the alias of a key, first use
-- DeleteAlias to delete the current alias and then use CreateAlias to
-- create a new alias. To associate an existing alias with a different key,
-- call UpdateAlias.
--
-- __Cross-account use:__ This operation can\'t be used across different
-- Amazon Web Services accounts.
--
-- __Related operations:__
--
-- -   CreateAlias
--
-- -   GetAlias
--
-- -   ListAliases
--
-- -   UpdateAlias
module Amazonka.PaymentCryptography.DeleteAlias
  ( -- * Creating a Request
    DeleteAlias (..),
    newDeleteAlias,

    -- * Request Lenses
    deleteAlias_aliasName,

    -- * Destructuring the Response
    DeleteAliasResponse (..),
    newDeleteAliasResponse,

    -- * Response Lenses
    deleteAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | A friendly name that you can use to refer Amazon Web Services Payment
    -- Cryptography key. This value must begin with @alias\/@ followed by a
    -- name, such as @alias\/ExampleAlias@.
    aliasName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'deleteAlias_aliasName' - A friendly name that you can use to refer Amazon Web Services Payment
-- Cryptography key. This value must begin with @alias\/@ followed by a
-- name, such as @alias\/ExampleAlias@.
newDeleteAlias ::
  -- | 'aliasName'
  Prelude.Text ->
  DeleteAlias
newDeleteAlias pAliasName_ =
  DeleteAlias' {aliasName = pAliasName_}

-- | A friendly name that you can use to refer Amazon Web Services Payment
-- Cryptography key. This value must begin with @alias\/@ followed by a
-- name, such as @alias\/ExampleAlias@.
deleteAlias_aliasName :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_aliasName = Lens.lens (\DeleteAlias' {aliasName} -> aliasName) (\s@DeleteAlias' {} a -> s {aliasName = a} :: DeleteAlias)

instance Core.AWSRequest DeleteAlias where
  type AWSResponse DeleteAlias = DeleteAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAlias where
  hashWithSalt _salt DeleteAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasName

instance Prelude.NFData DeleteAlias where
  rnf DeleteAlias' {..} = Prelude.rnf aliasName

instance Data.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PaymentCryptographyControlPlane.DeleteAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasName" Data..= aliasName)]
      )

instance Data.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAliasResponse_httpStatus' - The response's http status code.
newDeleteAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAliasResponse
newDeleteAliasResponse pHttpStatus_ =
  DeleteAliasResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAliasResponse_httpStatus :: Lens.Lens' DeleteAliasResponse Prelude.Int
deleteAliasResponse_httpStatus = Lens.lens (\DeleteAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteAliasResponse' {} a -> s {httpStatus = a} :: DeleteAliasResponse)

instance Prelude.NFData DeleteAliasResponse where
  rnf DeleteAliasResponse' {..} = Prelude.rnf httpStatus
