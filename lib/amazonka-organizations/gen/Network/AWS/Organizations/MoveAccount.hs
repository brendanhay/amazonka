{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.MoveAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves an account from its current source parent root or organizational unit (OU) to the specified destination parent root or OU.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.MoveAccount
  ( -- * Creating a request
    MoveAccount (..),
    mkMoveAccount,

    -- ** Request lenses
    maSourceParentId,
    maAccountId,
    maDestinationParentId,

    -- * Destructuring the response
    MoveAccountResponse (..),
    mkMoveAccountResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkMoveAccount' smart constructor.
data MoveAccount = MoveAccount'
  { -- | The unique identifier (ID) of the root or organizational unit that you want to move the account from.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
    --
    --     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    sourceParentId :: Lude.Text,
    -- | The unique identifier (ID) of the account that you want to move.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
    accountId :: Lude.Text,
    -- | The unique identifier (ID) of the root or organizational unit that you want to move the account to.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
    --
    --     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
    --
    --
    --     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
    destinationParentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MoveAccount' with the minimum fields required to make a request.
--
-- * 'sourceParentId' - The unique identifier (ID) of the root or organizational unit that you want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
-- * 'accountId' - The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
-- * 'destinationParentId' - The unique identifier (ID) of the root or organizational unit that you want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkMoveAccount ::
  -- | 'sourceParentId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'destinationParentId'
  Lude.Text ->
  MoveAccount
mkMoveAccount pSourceParentId_ pAccountId_ pDestinationParentId_ =
  MoveAccount'
    { sourceParentId = pSourceParentId_,
      accountId = pAccountId_,
      destinationParentId = pDestinationParentId_
    }

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account from.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'sourceParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maSourceParentId :: Lens.Lens' MoveAccount Lude.Text
maSourceParentId = Lens.lens (sourceParentId :: MoveAccount -> Lude.Text) (\s a -> s {sourceParentId = a} :: MoveAccount)
{-# DEPRECATED maSourceParentId "Use generic-lens or generic-optics with 'sourceParentId' instead." #-}

-- | The unique identifier (ID) of the account that you want to move.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maAccountId :: Lens.Lens' MoveAccount Lude.Text
maAccountId = Lens.lens (accountId :: MoveAccount -> Lude.Text) (\s a -> s {accountId = a} :: MoveAccount)
{-# DEPRECATED maAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The unique identifier (ID) of the root or organizational unit that you want to move the account to.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'destinationParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDestinationParentId :: Lens.Lens' MoveAccount Lude.Text
maDestinationParentId = Lens.lens (destinationParentId :: MoveAccount -> Lude.Text) (\s a -> s {destinationParentId = a} :: MoveAccount)
{-# DEPRECATED maDestinationParentId "Use generic-lens or generic-optics with 'destinationParentId' instead." #-}

instance Lude.AWSRequest MoveAccount where
  type Rs MoveAccount = MoveAccountResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull MoveAccountResponse'

instance Lude.ToHeaders MoveAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.MoveAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MoveAccount where
  toJSON MoveAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SourceParentId" Lude..= sourceParentId),
            Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("DestinationParentId" Lude..= destinationParentId)
          ]
      )

instance Lude.ToPath MoveAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery MoveAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkMoveAccountResponse' smart constructor.
data MoveAccountResponse = MoveAccountResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MoveAccountResponse' with the minimum fields required to make a request.
mkMoveAccountResponse ::
  MoveAccountResponse
mkMoveAccountResponse = MoveAccountResponse'
