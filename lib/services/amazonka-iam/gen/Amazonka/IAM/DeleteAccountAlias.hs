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
-- Module      : Amazonka.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Web Services account alias. For information
-- about using an Amazon Web Services account alias, see
-- <https://docs.aws.amazon.com/signin/latest/userguide/CreateAccountAlias.html Creating, deleting, and listing an Amazon Web Services account alias>
-- in the /Amazon Web Services Sign-In User Guide/.
module Amazonka.IAM.DeleteAccountAlias
  ( -- * Creating a Request
    DeleteAccountAlias (..),
    newDeleteAccountAlias,

    -- * Request Lenses
    deleteAccountAlias_accountAlias,

    -- * Destructuring the Response
    DeleteAccountAliasResponse (..),
    newDeleteAccountAliasResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccountAlias' smart constructor.
data DeleteAccountAlias = DeleteAccountAlias'
  { -- | The name of the account alias to delete.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of lowercase letters, digits, and dashes. You cannot start or
    -- finish with a dash, nor can you have two dashes in a row.
    accountAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAlias', 'deleteAccountAlias_accountAlias' - The name of the account alias to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of lowercase letters, digits, and dashes. You cannot start or
-- finish with a dash, nor can you have two dashes in a row.
newDeleteAccountAlias ::
  -- | 'accountAlias'
  Prelude.Text ->
  DeleteAccountAlias
newDeleteAccountAlias pAccountAlias_ =
  DeleteAccountAlias' {accountAlias = pAccountAlias_}

-- | The name of the account alias to delete.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of lowercase letters, digits, and dashes. You cannot start or
-- finish with a dash, nor can you have two dashes in a row.
deleteAccountAlias_accountAlias :: Lens.Lens' DeleteAccountAlias Prelude.Text
deleteAccountAlias_accountAlias = Lens.lens (\DeleteAccountAlias' {accountAlias} -> accountAlias) (\s@DeleteAccountAlias' {} a -> s {accountAlias = a} :: DeleteAccountAlias)

instance Core.AWSRequest DeleteAccountAlias where
  type
    AWSResponse DeleteAccountAlias =
      DeleteAccountAliasResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteAccountAliasResponse'

instance Prelude.Hashable DeleteAccountAlias where
  hashWithSalt _salt DeleteAccountAlias' {..} =
    _salt `Prelude.hashWithSalt` accountAlias

instance Prelude.NFData DeleteAccountAlias where
  rnf DeleteAccountAlias' {..} =
    Prelude.rnf accountAlias

instance Data.ToHeaders DeleteAccountAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAccountAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAccountAlias where
  toQuery DeleteAccountAlias' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteAccountAlias" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "AccountAlias" Data.=: accountAlias
      ]

-- | /See:/ 'newDeleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountAliasResponse ::
  DeleteAccountAliasResponse
newDeleteAccountAliasResponse =
  DeleteAccountAliasResponse'

instance Prelude.NFData DeleteAccountAliasResponse where
  rnf _ = ()
