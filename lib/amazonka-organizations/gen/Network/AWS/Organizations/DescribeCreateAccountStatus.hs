{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of an asynchronous request to create an account.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeCreateAccountStatus
  ( -- * Creating a request
    DescribeCreateAccountStatus (..),
    mkDescribeCreateAccountStatus,

    -- ** Request lenses
    dcasCreateAccountRequestId,

    -- * Destructuring the response
    DescribeCreateAccountStatusResponse (..),
    mkDescribeCreateAccountStatusResponse,

    -- ** Response lenses
    dcasrsCreateAccountStatus,
    dcasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCreateAccountStatus' smart constructor.
newtype DescribeCreateAccountStatus = DescribeCreateAccountStatus'
  { -- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
    createAccountRequestId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCreateAccountStatus' with the minimum fields required to make a request.
--
-- * 'createAccountRequestId' - Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
mkDescribeCreateAccountStatus ::
  -- | 'createAccountRequestId'
  Lude.Text ->
  DescribeCreateAccountStatus
mkDescribeCreateAccountStatus pCreateAccountRequestId_ =
  DescribeCreateAccountStatus'
    { createAccountRequestId =
        pCreateAccountRequestId_
    }

-- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'createAccountRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasCreateAccountRequestId :: Lens.Lens' DescribeCreateAccountStatus Lude.Text
dcasCreateAccountRequestId = Lens.lens (createAccountRequestId :: DescribeCreateAccountStatus -> Lude.Text) (\s a -> s {createAccountRequestId = a} :: DescribeCreateAccountStatus)
{-# DEPRECATED dcasCreateAccountRequestId "Use generic-lens or generic-optics with 'createAccountRequestId' instead." #-}

instance Lude.AWSRequest DescribeCreateAccountStatus where
  type
    Rs DescribeCreateAccountStatus =
      DescribeCreateAccountStatusResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCreateAccountStatusResponse'
            Lude.<$> (x Lude..?> "CreateAccountStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCreateAccountStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DescribeCreateAccountStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCreateAccountStatus where
  toJSON DescribeCreateAccountStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("CreateAccountRequestId" Lude..= createAccountRequestId)
          ]
      )

instance Lude.ToPath DescribeCreateAccountStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCreateAccountStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { -- | A structure that contains the current status of an account creation request.
    createAccountStatus :: Lude.Maybe CreateAccountStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCreateAccountStatusResponse' with the minimum fields required to make a request.
--
-- * 'createAccountStatus' - A structure that contains the current status of an account creation request.
-- * 'responseStatus' - The response status code.
mkDescribeCreateAccountStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCreateAccountStatusResponse
mkDescribeCreateAccountStatusResponse pResponseStatus_ =
  DescribeCreateAccountStatusResponse'
    { createAccountStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains the current status of an account creation request.
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrsCreateAccountStatus :: Lens.Lens' DescribeCreateAccountStatusResponse (Lude.Maybe CreateAccountStatus)
dcasrsCreateAccountStatus = Lens.lens (createAccountStatus :: DescribeCreateAccountStatusResponse -> Lude.Maybe CreateAccountStatus) (\s a -> s {createAccountStatus = a} :: DescribeCreateAccountStatusResponse)
{-# DEPRECATED dcasrsCreateAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrsResponseStatus :: Lens.Lens' DescribeCreateAccountStatusResponse Lude.Int
dcasrsResponseStatus = Lens.lens (responseStatus :: DescribeCreateAccountStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCreateAccountStatusResponse)
{-# DEPRECATED dcasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
