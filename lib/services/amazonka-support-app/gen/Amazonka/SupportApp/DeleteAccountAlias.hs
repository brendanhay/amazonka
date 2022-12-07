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
-- Module      : Amazonka.SupportApp.DeleteAccountAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias for an Amazon Web Services account ID. The alias
-- appears in the Amazon Web Services Support App page of the Amazon Web
-- Services Support Center. The alias also appears in Slack messages from
-- the Amazon Web Services Support App.
module Amazonka.SupportApp.DeleteAccountAlias
  ( -- * Creating a Request
    DeleteAccountAlias (..),
    newDeleteAccountAlias,

    -- * Destructuring the Response
    DeleteAccountAliasResponse (..),
    newDeleteAccountAliasResponse,

    -- * Response Lenses
    deleteAccountAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newDeleteAccountAlias' smart constructor.
data DeleteAccountAlias = DeleteAccountAlias'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountAlias ::
  DeleteAccountAlias
newDeleteAccountAlias = DeleteAccountAlias'

instance Core.AWSRequest DeleteAccountAlias where
  type
    AWSResponse DeleteAccountAlias =
      DeleteAccountAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccountAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccountAlias where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DeleteAccountAlias where
  rnf _ = ()

instance Data.ToHeaders DeleteAccountAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAccountAlias where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteAccountAlias where
  toPath =
    Prelude.const "/control/delete-account-alias"

instance Data.ToQuery DeleteAccountAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccountAliasResponse_httpStatus' - The response's http status code.
newDeleteAccountAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccountAliasResponse
newDeleteAccountAliasResponse pHttpStatus_ =
  DeleteAccountAliasResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAccountAliasResponse_httpStatus :: Lens.Lens' DeleteAccountAliasResponse Prelude.Int
deleteAccountAliasResponse_httpStatus = Lens.lens (\DeleteAccountAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteAccountAliasResponse' {} a -> s {httpStatus = a} :: DeleteAccountAliasResponse)

instance Prelude.NFData DeleteAccountAliasResponse where
  rnf DeleteAccountAliasResponse' {..} =
    Prelude.rnf httpStatus
