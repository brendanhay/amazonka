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
-- Module      : Amazonka.MemoryDb.UpdateACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the list of users that belong to the Access Control List.
module Amazonka.MemoryDb.UpdateACL
  ( -- * Creating a Request
    UpdateACL (..),
    newUpdateACL,

    -- * Request Lenses
    updateACL_userNamesToAdd,
    updateACL_userNamesToRemove,
    updateACL_aCLName,

    -- * Destructuring the Response
    UpdateACLResponse (..),
    newUpdateACLResponse,

    -- * Response Lenses
    updateACLResponse_acl,
    updateACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateACL' smart constructor.
data UpdateACL = UpdateACL'
  { -- | The list of users to add to the Access Control List
    userNamesToAdd :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of users to remove from the Access Control List
    userNamesToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the Access Control List
    aCLName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userNamesToAdd', 'updateACL_userNamesToAdd' - The list of users to add to the Access Control List
--
-- 'userNamesToRemove', 'updateACL_userNamesToRemove' - The list of users to remove from the Access Control List
--
-- 'aCLName', 'updateACL_aCLName' - The name of the Access Control List
newUpdateACL ::
  -- | 'aCLName'
  Prelude.Text ->
  UpdateACL
newUpdateACL pACLName_ =
  UpdateACL'
    { userNamesToAdd = Prelude.Nothing,
      userNamesToRemove = Prelude.Nothing,
      aCLName = pACLName_
    }

-- | The list of users to add to the Access Control List
updateACL_userNamesToAdd :: Lens.Lens' UpdateACL (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateACL_userNamesToAdd = Lens.lens (\UpdateACL' {userNamesToAdd} -> userNamesToAdd) (\s@UpdateACL' {} a -> s {userNamesToAdd = a} :: UpdateACL) Prelude.. Lens.mapping Lens.coerced

-- | The list of users to remove from the Access Control List
updateACL_userNamesToRemove :: Lens.Lens' UpdateACL (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateACL_userNamesToRemove = Lens.lens (\UpdateACL' {userNamesToRemove} -> userNamesToRemove) (\s@UpdateACL' {} a -> s {userNamesToRemove = a} :: UpdateACL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Access Control List
updateACL_aCLName :: Lens.Lens' UpdateACL Prelude.Text
updateACL_aCLName = Lens.lens (\UpdateACL' {aCLName} -> aCLName) (\s@UpdateACL' {} a -> s {aCLName = a} :: UpdateACL)

instance Core.AWSRequest UpdateACL where
  type AWSResponse UpdateACL = UpdateACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateACLResponse'
            Prelude.<$> (x Data..?> "ACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateACL where
  hashWithSalt _salt UpdateACL' {..} =
    _salt `Prelude.hashWithSalt` userNamesToAdd
      `Prelude.hashWithSalt` userNamesToRemove
      `Prelude.hashWithSalt` aCLName

instance Prelude.NFData UpdateACL where
  rnf UpdateACL' {..} =
    Prelude.rnf userNamesToAdd
      `Prelude.seq` Prelude.rnf userNamesToRemove
      `Prelude.seq` Prelude.rnf aCLName

instance Data.ToHeaders UpdateACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonMemoryDB.UpdateACL" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateACL where
  toJSON UpdateACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UserNamesToAdd" Data..=)
              Prelude.<$> userNamesToAdd,
            ("UserNamesToRemove" Data..=)
              Prelude.<$> userNamesToRemove,
            Prelude.Just ("ACLName" Data..= aCLName)
          ]
      )

instance Data.ToPath UpdateACL where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateACLResponse' smart constructor.
data UpdateACLResponse = UpdateACLResponse'
  { -- | The updated Access Control List
    acl :: Prelude.Maybe ACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acl', 'updateACLResponse_acl' - The updated Access Control List
--
-- 'httpStatus', 'updateACLResponse_httpStatus' - The response's http status code.
newUpdateACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateACLResponse
newUpdateACLResponse pHttpStatus_ =
  UpdateACLResponse'
    { acl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated Access Control List
updateACLResponse_acl :: Lens.Lens' UpdateACLResponse (Prelude.Maybe ACL)
updateACLResponse_acl = Lens.lens (\UpdateACLResponse' {acl} -> acl) (\s@UpdateACLResponse' {} a -> s {acl = a} :: UpdateACLResponse)

-- | The response's http status code.
updateACLResponse_httpStatus :: Lens.Lens' UpdateACLResponse Prelude.Int
updateACLResponse_httpStatus = Lens.lens (\UpdateACLResponse' {httpStatus} -> httpStatus) (\s@UpdateACLResponse' {} a -> s {httpStatus = a} :: UpdateACLResponse)

instance Prelude.NFData UpdateACLResponse where
  rnf UpdateACLResponse' {..} =
    Prelude.rnf acl
      `Prelude.seq` Prelude.rnf httpStatus
