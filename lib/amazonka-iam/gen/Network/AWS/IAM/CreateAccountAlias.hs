{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your AWS account. For information about using an AWS account alias, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateAccountAlias
  ( -- * Creating a request
    CreateAccountAlias (..),
    mkCreateAccountAlias,

    -- ** Request lenses
    caaAccountAlias,

    -- * Destructuring the response
    CreateAccountAliasResponse (..),
    mkCreateAccountAliasResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAccountAlias' smart constructor.
newtype CreateAccountAlias = CreateAccountAlias'
  { -- | The account alias to create.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
    accountAlias :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccountAlias' with the minimum fields required to make a request.
--
-- * 'accountAlias' - The account alias to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
mkCreateAccountAlias ::
  -- | 'accountAlias'
  Lude.Text ->
  CreateAccountAlias
mkCreateAccountAlias pAccountAlias_ =
  CreateAccountAlias' {accountAlias = pAccountAlias_}

-- | The account alias to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of lowercase letters, digits, and dashes. You cannot start or finish with a dash, nor can you have two dashes in a row.
--
-- /Note:/ Consider using 'accountAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAccountAlias :: Lens.Lens' CreateAccountAlias Lude.Text
caaAccountAlias = Lens.lens (accountAlias :: CreateAccountAlias -> Lude.Text) (\s a -> s {accountAlias = a} :: CreateAccountAlias)
{-# DEPRECATED caaAccountAlias "Use generic-lens or generic-optics with 'accountAlias' instead." #-}

instance Lude.AWSRequest CreateAccountAlias where
  type Rs CreateAccountAlias = CreateAccountAliasResponse
  request = Req.postQuery iamService
  response = Res.receiveNull CreateAccountAliasResponse'

instance Lude.ToHeaders CreateAccountAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateAccountAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAccountAlias where
  toQuery CreateAccountAlias' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateAccountAlias" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "AccountAlias" Lude.=: accountAlias
      ]

-- | /See:/ 'mkCreateAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse = CreateAccountAliasResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccountAliasResponse' with the minimum fields required to make a request.
mkCreateAccountAliasResponse ::
  CreateAccountAliasResponse
mkCreateAccountAliasResponse = CreateAccountAliasResponse'
