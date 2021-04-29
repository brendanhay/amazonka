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
-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS account alias. For information about using an
-- AWS account alias, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an alias for your AWS account ID>
-- in the /IAM User Guide/.
module Network.AWS.IAM.DeleteAccountAlias
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteAccountAlias where
  type
    Rs DeleteAccountAlias =
      DeleteAccountAliasResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteAccountAliasResponse'

instance Prelude.Hashable DeleteAccountAlias

instance Prelude.NFData DeleteAccountAlias

instance Prelude.ToHeaders DeleteAccountAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteAccountAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAccountAlias where
  toQuery DeleteAccountAlias' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteAccountAlias" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "AccountAlias" Prelude.=: accountAlias
      ]

-- | /See:/ 'newDeleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccountAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAccountAliasResponse ::
  DeleteAccountAliasResponse
newDeleteAccountAliasResponse =
  DeleteAccountAliasResponse'

instance Prelude.NFData DeleteAccountAliasResponse
