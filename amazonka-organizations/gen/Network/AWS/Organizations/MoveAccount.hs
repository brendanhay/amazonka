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
-- Module      : Network.AWS.Organizations.MoveAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an account from its current source parent root or organizational
-- unit (OU) to the specified destination parent root or OU.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.MoveAccount
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest MoveAccount where
  type Rs MoveAccount = MoveAccountResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull MoveAccountResponse'

instance Prelude.Hashable MoveAccount

instance Prelude.NFData MoveAccount

instance Prelude.ToHeaders MoveAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.MoveAccount" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON MoveAccount where
  toJSON MoveAccount' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just
              ("SourceParentId" Prelude..= sourceParentId),
            Prelude.Just
              ( "DestinationParentId"
                  Prelude..= destinationParentId
              )
          ]
      )

instance Prelude.ToPath MoveAccount where
  toPath = Prelude.const "/"

instance Prelude.ToQuery MoveAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMoveAccountResponse' smart constructor.
data MoveAccountResponse = MoveAccountResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MoveAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newMoveAccountResponse ::
  MoveAccountResponse
newMoveAccountResponse = MoveAccountResponse'

instance Prelude.NFData MoveAccountResponse
