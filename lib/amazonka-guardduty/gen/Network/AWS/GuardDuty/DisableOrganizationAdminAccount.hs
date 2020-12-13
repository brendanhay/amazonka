{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an AWS account within the Organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.DisableOrganizationAdminAccount
  ( -- * Creating a request
    DisableOrganizationAdminAccount (..),
    mkDisableOrganizationAdminAccount,

    -- ** Request lenses
    doaaAdminAccountId,

    -- * Destructuring the response
    DisableOrganizationAdminAccountResponse (..),
    mkDisableOrganizationAdminAccountResponse,

    -- ** Response lenses
    doaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableOrganizationAdminAccount' smart constructor.
newtype DisableOrganizationAdminAccount = DisableOrganizationAdminAccount'
  { -- | The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
    adminAccountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableOrganizationAdminAccount' with the minimum fields required to make a request.
--
-- * 'adminAccountId' - The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
mkDisableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Lude.Text ->
  DisableOrganizationAdminAccount
mkDisableOrganizationAdminAccount pAdminAccountId_ =
  DisableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organizations account to be disabled as a GuardDuty delegated administrator.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doaaAdminAccountId :: Lens.Lens' DisableOrganizationAdminAccount Lude.Text
doaaAdminAccountId = Lens.lens (adminAccountId :: DisableOrganizationAdminAccount -> Lude.Text) (\s a -> s {adminAccountId = a} :: DisableOrganizationAdminAccount)
{-# DEPRECATED doaaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

instance Lude.AWSRequest DisableOrganizationAdminAccount where
  type
    Rs DisableOrganizationAdminAccount =
      DisableOrganizationAdminAccountResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableOrganizationAdminAccountResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableOrganizationAdminAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableOrganizationAdminAccount where
  toJSON DisableOrganizationAdminAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("adminAccountId" Lude..= adminAccountId)]
      )

instance Lude.ToPath DisableOrganizationAdminAccount where
  toPath = Lude.const "/admin/disable"

instance Lude.ToQuery DisableOrganizationAdminAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableOrganizationAdminAccountResponse' smart constructor.
newtype DisableOrganizationAdminAccountResponse = DisableOrganizationAdminAccountResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableOrganizationAdminAccountResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableOrganizationAdminAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableOrganizationAdminAccountResponse
mkDisableOrganizationAdminAccountResponse pResponseStatus_ =
  DisableOrganizationAdminAccountResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doaarsResponseStatus :: Lens.Lens' DisableOrganizationAdminAccountResponse Lude.Int
doaarsResponseStatus = Lens.lens (responseStatus :: DisableOrganizationAdminAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableOrganizationAdminAccountResponse)
{-# DEPRECATED doaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
