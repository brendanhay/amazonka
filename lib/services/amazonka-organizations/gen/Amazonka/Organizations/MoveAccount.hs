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
-- Module      : Amazonka.Organizations.MoveAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an account from its current source parent root or organizational
-- unit (OU) to the specified destination parent root or OU.
--
-- This operation can be called only from the organization\'s management
-- account.
module Amazonka.Organizations.MoveAccount
  ( -- * Creating a Request
    MoveAccount (..),
    newMoveAccount,

    -- * Request Lenses
    moveAccount_accountId,
    moveAccount_sourceParentId,
    moveAccount_destinationParentId,

    -- * Destructuring the Response
    MoveAccountResponse (..),
    newMoveAccountResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMoveAccount' smart constructor.
data MoveAccount = MoveAccount'
  { -- | The unique identifier (ID) of the account that you want to move.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    accountId :: Prelude.Text,
    -- | The unique identifier (ID) of the root or organizational unit that you
    -- want to move the account from.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    sourceParentId :: Prelude.Text,
    -- | The unique identifier (ID) of the root or organizational unit that you
    -- want to move the account to.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
    -- string requires one of the following:
    --
    -- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
    --     lowercase letters or digits.
    --
    -- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
    --     followed by from 4 to 32 lowercase letters or digits (the ID of the
    --     root that the OU is in). This string is followed by a second \"-\"
    --     dash and from 8 to 32 additional lowercase letters or digits.
    destinationParentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'moveAccount_accountId' - The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
--
-- 'sourceParentId', 'moveAccount_sourceParentId' - The unique identifier (ID) of the root or organizational unit that you
-- want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
--
-- 'destinationParentId', 'moveAccount_destinationParentId' - The unique identifier (ID) of the root or organizational unit that you
-- want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
newMoveAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'sourceParentId'
  Prelude.Text ->
  -- | 'destinationParentId'
  Prelude.Text ->
  MoveAccount
newMoveAccount
  pAccountId_
  pSourceParentId_
  pDestinationParentId_ =
    MoveAccount'
      { accountId = pAccountId_,
        sourceParentId = pSourceParentId_,
        destinationParentId = pDestinationParentId_
      }

-- | The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
moveAccount_accountId :: Lens.Lens' MoveAccount Prelude.Text
moveAccount_accountId = Lens.lens (\MoveAccount' {accountId} -> accountId) (\s@MoveAccount' {} a -> s {accountId = a} :: MoveAccount)

-- | The unique identifier (ID) of the root or organizational unit that you
-- want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
moveAccount_sourceParentId :: Lens.Lens' MoveAccount Prelude.Text
moveAccount_sourceParentId = Lens.lens (\MoveAccount' {sourceParentId} -> sourceParentId) (\s@MoveAccount' {} a -> s {sourceParentId = a} :: MoveAccount)

-- | The unique identifier (ID) of the root or organizational unit that you
-- want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID
-- string requires one of the following:
--
-- -   __Root__ - A string that begins with \"r-\" followed by from 4 to 32
--     lowercase letters or digits.
--
-- -   __Organizational unit (OU)__ - A string that begins with \"ou-\"
--     followed by from 4 to 32 lowercase letters or digits (the ID of the
--     root that the OU is in). This string is followed by a second \"-\"
--     dash and from 8 to 32 additional lowercase letters or digits.
moveAccount_destinationParentId :: Lens.Lens' MoveAccount Prelude.Text
moveAccount_destinationParentId = Lens.lens (\MoveAccount' {destinationParentId} -> destinationParentId) (\s@MoveAccount' {} a -> s {destinationParentId = a} :: MoveAccount)

instance Core.AWSRequest MoveAccount where
  type AWSResponse MoveAccount = MoveAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull MoveAccountResponse'

instance Prelude.Hashable MoveAccount where
  hashWithSalt _salt MoveAccount' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` sourceParentId
      `Prelude.hashWithSalt` destinationParentId

instance Prelude.NFData MoveAccount where
  rnf MoveAccount' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf sourceParentId `Prelude.seq`
        Prelude.rnf destinationParentId

instance Data.ToHeaders MoveAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSOrganizationsV20161128.MoveAccount" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MoveAccount where
  toJSON MoveAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just
              ("SourceParentId" Data..= sourceParentId),
            Prelude.Just
              ("DestinationParentId" Data..= destinationParentId)
          ]
      )

instance Data.ToPath MoveAccount where
  toPath = Prelude.const "/"

instance Data.ToQuery MoveAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMoveAccountResponse' smart constructor.
data MoveAccountResponse = MoveAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MoveAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newMoveAccountResponse ::
  MoveAccountResponse
newMoveAccountResponse = MoveAccountResponse'

instance Prelude.NFData MoveAccountResponse where
  rnf _ = ()
