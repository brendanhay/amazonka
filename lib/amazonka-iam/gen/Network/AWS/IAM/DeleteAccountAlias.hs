{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteAccountAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS account alias. For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteAccountAlias
  ( -- * Creating a request
    DeleteAccountAlias (..),
    mkDeleteAccountAlias,

    -- ** Request lenses
    daaAccountAlias,

    -- * Destructuring the response
    DeleteAccountAliasResponse (..),
    mkDeleteAccountAliasResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAccountAlias' smart constructor.
newtype DeleteAccountAlias = DeleteAccountAlias'
  { -- | The name of the account alias to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
    accountAlias :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountAlias' with the minimum fields required to make a request.
--
-- * 'accountAlias' - The name of the account alias to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
mkDeleteAccountAlias ::
  -- | 'accountAlias'
  Lude.Text ->
  DeleteAccountAlias
mkDeleteAccountAlias pAccountAlias_ =
  DeleteAccountAlias' {accountAlias = pAccountAlias_}

-- | The name of the account alias to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAccountAlias :: Lens.Lens' DeleteAccountAlias Lude.Text
daaAccountAlias = Lens.lens (accountAlias :: DeleteAccountAlias -> Lude.Text) (\s a -> s {accountAlias = a} :: DeleteAccountAlias)
{-# DEPRECATED daaAccountAlias "Use generic-lens or generic-optics with 'accountAlias' instead." #-}

instance Lude.AWSRequest DeleteAccountAlias where
  type Rs DeleteAccountAlias = DeleteAccountAliasResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteAccountAliasResponse'

instance Lude.ToHeaders DeleteAccountAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAccountAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAccountAlias where
  toQuery DeleteAccountAlias' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAccountAlias" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "AccountAlias" Lude.=: accountAlias
      ]

-- | /See:/ 'mkDeleteAccountAliasResponse' smart constructor.
data DeleteAccountAliasResponse = DeleteAccountAliasResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAccountAliasResponse' with the minimum fields required to make a request.
mkDeleteAccountAliasResponse ::
  DeleteAccountAliasResponse
mkDeleteAccountAliasResponse = DeleteAccountAliasResponse'
