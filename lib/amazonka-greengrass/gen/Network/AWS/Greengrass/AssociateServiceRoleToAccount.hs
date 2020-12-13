{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with your account. AWS IoT Greengrass will use the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. The role must have at least minimum permissions in the policy ''AWSGreengrassResourceAccessRolePolicy''.
module Network.AWS.Greengrass.AssociateServiceRoleToAccount
  ( -- * Creating a request
    AssociateServiceRoleToAccount (..),
    mkAssociateServiceRoleToAccount,

    -- ** Request lenses
    asrtaRoleARN,

    -- * Destructuring the response
    AssociateServiceRoleToAccountResponse (..),
    mkAssociateServiceRoleToAccountResponse,

    -- ** Response lenses
    asrtarsAssociatedAt,
    asrtarsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateServiceRoleToAccount' smart constructor.
newtype AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { -- | The ARN of the service role you wish to associate with your account.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateServiceRoleToAccount' with the minimum fields required to make a request.
--
-- * 'roleARN' - The ARN of the service role you wish to associate with your account.
mkAssociateServiceRoleToAccount ::
  -- | 'roleARN'
  Lude.Text ->
  AssociateServiceRoleToAccount
mkAssociateServiceRoleToAccount pRoleARN_ =
  AssociateServiceRoleToAccount' {roleARN = pRoleARN_}

-- | The ARN of the service role you wish to associate with your account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtaRoleARN :: Lens.Lens' AssociateServiceRoleToAccount Lude.Text
asrtaRoleARN = Lens.lens (roleARN :: AssociateServiceRoleToAccount -> Lude.Text) (\s a -> s {roleARN = a} :: AssociateServiceRoleToAccount)
{-# DEPRECATED asrtaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssociateServiceRoleToAccount where
  type
    Rs AssociateServiceRoleToAccount =
      AssociateServiceRoleToAccountResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssociateServiceRoleToAccountResponse'
            Lude.<$> (x Lude..?> "AssociatedAt") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateServiceRoleToAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateServiceRoleToAccount where
  toJSON AssociateServiceRoleToAccount' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RoleArn" Lude..= roleARN)])

instance Lude.ToPath AssociateServiceRoleToAccount where
  toPath = Lude.const "/greengrass/servicerole"

instance Lude.ToQuery AssociateServiceRoleToAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { -- | The time when the service role was associated with the account.
    associatedAt :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateServiceRoleToAccountResponse' with the minimum fields required to make a request.
--
-- * 'associatedAt' - The time when the service role was associated with the account.
-- * 'responseStatus' - The response status code.
mkAssociateServiceRoleToAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateServiceRoleToAccountResponse
mkAssociateServiceRoleToAccountResponse pResponseStatus_ =
  AssociateServiceRoleToAccountResponse'
    { associatedAt =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The time when the service role was associated with the account.
--
-- /Note:/ Consider using 'associatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtarsAssociatedAt :: Lens.Lens' AssociateServiceRoleToAccountResponse (Lude.Maybe Lude.Text)
asrtarsAssociatedAt = Lens.lens (associatedAt :: AssociateServiceRoleToAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {associatedAt = a} :: AssociateServiceRoleToAccountResponse)
{-# DEPRECATED asrtarsAssociatedAt "Use generic-lens or generic-optics with 'associatedAt' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrtarsResponseStatus :: Lens.Lens' AssociateServiceRoleToAccountResponse Lude.Int
asrtarsResponseStatus = Lens.lens (responseStatus :: AssociateServiceRoleToAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateServiceRoleToAccountResponse)
{-# DEPRECATED asrtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
