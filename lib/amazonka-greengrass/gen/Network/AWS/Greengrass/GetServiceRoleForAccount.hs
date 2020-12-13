{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetServiceRoleForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the service role that is attached to your account.
module Network.AWS.Greengrass.GetServiceRoleForAccount
  ( -- * Creating a request
    GetServiceRoleForAccount (..),
    mkGetServiceRoleForAccount,

    -- * Destructuring the response
    GetServiceRoleForAccountResponse (..),
    mkGetServiceRoleForAccountResponse,

    -- ** Response lenses
    gsrfarsAssociatedAt,
    gsrfarsRoleARN,
    gsrfarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetServiceRoleForAccount' smart constructor.
data GetServiceRoleForAccount = GetServiceRoleForAccount'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceRoleForAccount' with the minimum fields required to make a request.
mkGetServiceRoleForAccount ::
  GetServiceRoleForAccount
mkGetServiceRoleForAccount = GetServiceRoleForAccount'

instance Lude.AWSRequest GetServiceRoleForAccount where
  type Rs GetServiceRoleForAccount = GetServiceRoleForAccountResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetServiceRoleForAccountResponse'
            Lude.<$> (x Lude..?> "AssociatedAt")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetServiceRoleForAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetServiceRoleForAccount where
  toPath = Lude.const "/greengrass/servicerole"

instance Lude.ToQuery GetServiceRoleForAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetServiceRoleForAccountResponse' smart constructor.
data GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse'
  { -- | The time when the service role was associated with the account.
    associatedAt :: Lude.Maybe Lude.Text,
    -- | The ARN of the role which is associated with the account.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceRoleForAccountResponse' with the minimum fields required to make a request.
--
-- * 'associatedAt' - The time when the service role was associated with the account.
-- * 'roleARN' - The ARN of the role which is associated with the account.
-- * 'responseStatus' - The response status code.
mkGetServiceRoleForAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetServiceRoleForAccountResponse
mkGetServiceRoleForAccountResponse pResponseStatus_ =
  GetServiceRoleForAccountResponse'
    { associatedAt = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time when the service role was associated with the account.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarsAssociatedAt :: Lens.Lens' GetServiceRoleForAccountResponse (Lude.Maybe Lude.Text)
gsrfarsAssociatedAt = Lens.lens (associatedAt :: GetServiceRoleForAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {associatedAt = a} :: GetServiceRoleForAccountResponse)
{-# DEPRECATED gsrfarsAssociatedAt "Use generic-lens or generic-optics with 'associatedAt' instead." #-}

-- | The ARN of the role which is associated with the account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarsRoleARN :: Lens.Lens' GetServiceRoleForAccountResponse (Lude.Maybe Lude.Text)
gsrfarsRoleARN = Lens.lens (roleARN :: GetServiceRoleForAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GetServiceRoleForAccountResponse)
{-# DEPRECATED gsrfarsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfarsResponseStatus :: Lens.Lens' GetServiceRoleForAccountResponse Lude.Int
gsrfarsResponseStatus = Lens.lens (responseStatus :: GetServiceRoleForAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServiceRoleForAccountResponse)
{-# DEPRECATED gsrfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
