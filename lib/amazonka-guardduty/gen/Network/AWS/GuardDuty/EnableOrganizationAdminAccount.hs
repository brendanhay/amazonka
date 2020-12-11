{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.EnableOrganizationAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an AWS account within the organization as the GuardDuty delegated administrator.
module Network.AWS.GuardDuty.EnableOrganizationAdminAccount
  ( -- * Creating a request
    EnableOrganizationAdminAccount (..),
    mkEnableOrganizationAdminAccount,

    -- ** Request lenses
    eoaaAdminAccountId,

    -- * Destructuring the response
    EnableOrganizationAdminAccountResponse (..),
    mkEnableOrganizationAdminAccountResponse,

    -- ** Response lenses
    eoaarsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableOrganizationAdminAccount' smart constructor.
newtype EnableOrganizationAdminAccount = EnableOrganizationAdminAccount'
  { adminAccountId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableOrganizationAdminAccount' with the minimum fields required to make a request.
--
-- * 'adminAccountId' - The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
mkEnableOrganizationAdminAccount ::
  -- | 'adminAccountId'
  Lude.Text ->
  EnableOrganizationAdminAccount
mkEnableOrganizationAdminAccount pAdminAccountId_ =
  EnableOrganizationAdminAccount'
    { adminAccountId =
        pAdminAccountId_
    }

-- | The AWS Account ID for the organization account to be enabled as a GuardDuty delegated administrator.
--
-- /Note:/ Consider using 'adminAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaaAdminAccountId :: Lens.Lens' EnableOrganizationAdminAccount Lude.Text
eoaaAdminAccountId = Lens.lens (adminAccountId :: EnableOrganizationAdminAccount -> Lude.Text) (\s a -> s {adminAccountId = a} :: EnableOrganizationAdminAccount)
{-# DEPRECATED eoaaAdminAccountId "Use generic-lens or generic-optics with 'adminAccountId' instead." #-}

instance Lude.AWSRequest EnableOrganizationAdminAccount where
  type
    Rs EnableOrganizationAdminAccount =
      EnableOrganizationAdminAccountResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableOrganizationAdminAccountResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableOrganizationAdminAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableOrganizationAdminAccount where
  toJSON EnableOrganizationAdminAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("adminAccountId" Lude..= adminAccountId)]
      )

instance Lude.ToPath EnableOrganizationAdminAccount where
  toPath = Lude.const "/admin/enable"

instance Lude.ToQuery EnableOrganizationAdminAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableOrganizationAdminAccountResponse' smart constructor.
newtype EnableOrganizationAdminAccountResponse = EnableOrganizationAdminAccountResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableOrganizationAdminAccountResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableOrganizationAdminAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableOrganizationAdminAccountResponse
mkEnableOrganizationAdminAccountResponse pResponseStatus_ =
  EnableOrganizationAdminAccountResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eoaarsResponseStatus :: Lens.Lens' EnableOrganizationAdminAccountResponse Lude.Int
eoaarsResponseStatus = Lens.lens (responseStatus :: EnableOrganizationAdminAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableOrganizationAdminAccountResponse)
{-# DEPRECATED eoaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
