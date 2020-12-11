{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves AWS Organizations-related information about the specified account.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeAccount
  ( -- * Creating a request
    DescribeAccount (..),
    mkDescribeAccount,

    -- ** Request lenses
    daAccountId,

    -- * Destructuring the response
    DescribeAccountResponse (..),
    mkDescribeAccountResponse,

    -- ** Response lenses
    darsAccount,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccount' smart constructor.
newtype DescribeAccount = DescribeAccount' {accountId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccount' with the minimum fields required to make a request.
--
-- * 'accountId' - The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
mkDescribeAccount ::
  -- | 'accountId'
  Lude.Text ->
  DescribeAccount
mkDescribeAccount pAccountId_ =
  DescribeAccount' {accountId = pAccountId_}

-- | The unique identifier (ID) of the AWS account that you want information about. You can get the ID from the 'ListAccounts' or 'ListAccountsForParent' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAccountId :: Lens.Lens' DescribeAccount Lude.Text
daAccountId = Lens.lens (accountId :: DescribeAccount -> Lude.Text) (\s a -> s {accountId = a} :: DescribeAccount)
{-# DEPRECATED daAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest DescribeAccount where
  type Rs DescribeAccount = DescribeAccountResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Lude.<$> (x Lude..?> "Account") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DescribeAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAccount where
  toJSON DescribeAccount' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AccountId" Lude..= accountId)])

instance Lude.ToPath DescribeAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { account ::
      Lude.Maybe Account,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountResponse' with the minimum fields required to make a request.
--
-- * 'account' - A structure that contains information about the requested account.
-- * 'responseStatus' - The response status code.
mkDescribeAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountResponse
mkDescribeAccountResponse pResponseStatus_ =
  DescribeAccountResponse'
    { account = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains information about the requested account.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAccount :: Lens.Lens' DescribeAccountResponse (Lude.Maybe Account)
darsAccount = Lens.lens (account :: DescribeAccountResponse -> Lude.Maybe Account) (\s a -> s {account = a} :: DescribeAccountResponse)
{-# DEPRECATED darsAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAccountResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
