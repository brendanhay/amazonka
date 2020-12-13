{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the service role from your account. Without a service role, deployments will not work.
module Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
  ( -- * Creating a request
    DisassociateServiceRoleFromAccount (..),
    mkDisassociateServiceRoleFromAccount,

    -- * Destructuring the response
    DisassociateServiceRoleFromAccountResponse (..),
    mkDisassociateServiceRoleFromAccountResponse,

    -- ** Response lenses
    dsrfarsDisassociatedAt,
    dsrfarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateServiceRoleFromAccount' smart constructor.
data DisassociateServiceRoleFromAccount = DisassociateServiceRoleFromAccount'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateServiceRoleFromAccount' with the minimum fields required to make a request.
mkDisassociateServiceRoleFromAccount ::
  DisassociateServiceRoleFromAccount
mkDisassociateServiceRoleFromAccount =
  DisassociateServiceRoleFromAccount'

instance Lude.AWSRequest DisassociateServiceRoleFromAccount where
  type
    Rs DisassociateServiceRoleFromAccount =
      DisassociateServiceRoleFromAccountResponse
  request = Req.delete greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisassociateServiceRoleFromAccountResponse'
            Lude.<$> (x Lude..?> "DisassociatedAt")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateServiceRoleFromAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DisassociateServiceRoleFromAccount where
  toPath = Lude.const "/greengrass/servicerole"

instance Lude.ToQuery DisassociateServiceRoleFromAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateServiceRoleFromAccountResponse' smart constructor.
data DisassociateServiceRoleFromAccountResponse = DisassociateServiceRoleFromAccountResponse'
  { -- | The time when the service role was disassociated from the account.
    disassociatedAt :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateServiceRoleFromAccountResponse' with the minimum fields required to make a request.
--
-- * 'disassociatedAt' - The time when the service role was disassociated from the account.
-- * 'responseStatus' - The response status code.
mkDisassociateServiceRoleFromAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateServiceRoleFromAccountResponse
mkDisassociateServiceRoleFromAccountResponse pResponseStatus_ =
  DisassociateServiceRoleFromAccountResponse'
    { disassociatedAt =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time when the service role was disassociated from the account.
--
-- /Note:/ Consider using 'disassociatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfarsDisassociatedAt :: Lens.Lens' DisassociateServiceRoleFromAccountResponse (Lude.Maybe Lude.Text)
dsrfarsDisassociatedAt = Lens.lens (disassociatedAt :: DisassociateServiceRoleFromAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {disassociatedAt = a} :: DisassociateServiceRoleFromAccountResponse)
{-# DEPRECATED dsrfarsDisassociatedAt "Use generic-lens or generic-optics with 'disassociatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfarsResponseStatus :: Lens.Lens' DisassociateServiceRoleFromAccountResponse Lude.Int
dsrfarsResponseStatus = Lens.lens (responseStatus :: DisassociateServiceRoleFromAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateServiceRoleFromAccountResponse)
{-# DEPRECATED dsrfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
