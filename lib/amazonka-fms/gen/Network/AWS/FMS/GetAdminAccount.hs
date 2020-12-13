{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the AWS Organizations master account that is associated with AWS Firewall Manager as the AWS Firewall Manager administrator.
module Network.AWS.FMS.GetAdminAccount
  ( -- * Creating a request
    GetAdminAccount (..),
    mkGetAdminAccount,

    -- * Destructuring the response
    GetAdminAccountResponse (..),
    mkGetAdminAccountResponse,

    -- ** Response lenses
    gaarsAdminAccount,
    gaarsRoleStatus,
    gaarsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAdminAccount' smart constructor.
data GetAdminAccount = GetAdminAccount'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAdminAccount' with the minimum fields required to make a request.
mkGetAdminAccount ::
  GetAdminAccount
mkGetAdminAccount = GetAdminAccount'

instance Lude.AWSRequest GetAdminAccount where
  type Rs GetAdminAccount = GetAdminAccountResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAdminAccountResponse'
            Lude.<$> (x Lude..?> "AdminAccount")
            Lude.<*> (x Lude..?> "RoleStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAdminAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetAdminAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAdminAccount where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetAdminAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAdminAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { -- | The AWS account that is set as the AWS Firewall Manager administrator.
    adminAccount :: Lude.Maybe Lude.Text,
    -- | The status of the AWS account that you set as the AWS Firewall Manager administrator.
    roleStatus :: Lude.Maybe AccountRoleStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAdminAccountResponse' with the minimum fields required to make a request.
--
-- * 'adminAccount' - The AWS account that is set as the AWS Firewall Manager administrator.
-- * 'roleStatus' - The status of the AWS account that you set as the AWS Firewall Manager administrator.
-- * 'responseStatus' - The response status code.
mkGetAdminAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAdminAccountResponse
mkGetAdminAccountResponse pResponseStatus_ =
  GetAdminAccountResponse'
    { adminAccount = Lude.Nothing,
      roleStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS account that is set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'adminAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarsAdminAccount :: Lens.Lens' GetAdminAccountResponse (Lude.Maybe Lude.Text)
gaarsAdminAccount = Lens.lens (adminAccount :: GetAdminAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {adminAccount = a} :: GetAdminAccountResponse)
{-# DEPRECATED gaarsAdminAccount "Use generic-lens or generic-optics with 'adminAccount' instead." #-}

-- | The status of the AWS account that you set as the AWS Firewall Manager administrator.
--
-- /Note:/ Consider using 'roleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarsRoleStatus :: Lens.Lens' GetAdminAccountResponse (Lude.Maybe AccountRoleStatus)
gaarsRoleStatus = Lens.lens (roleStatus :: GetAdminAccountResponse -> Lude.Maybe AccountRoleStatus) (\s a -> s {roleStatus = a} :: GetAdminAccountResponse)
{-# DEPRECATED gaarsRoleStatus "Use generic-lens or generic-optics with 'roleStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaarsResponseStatus :: Lens.Lens' GetAdminAccountResponse Lude.Int
gaarsResponseStatus = Lens.lens (responseStatus :: GetAdminAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAdminAccountResponse)
{-# DEPRECATED gaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
