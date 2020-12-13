{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.DescribeAccount
  ( -- * Creating a request
    DescribeAccount (..),
    mkDescribeAccount,

    -- * Destructuring the response
    DescribeAccountResponse (..),
    mkDescribeAccountResponse,

    -- ** Response lenses
    darsDedicatedTenancySupport,
    darsDedicatedTenancyManagementCidrRange,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeAccount' smart constructor.
data DescribeAccount = DescribeAccount'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccount' with the minimum fields required to make a request.
mkDescribeAccount ::
  DescribeAccount
mkDescribeAccount = DescribeAccount'

instance Lude.AWSRequest DescribeAccount where
  type Rs DescribeAccount = DescribeAccountResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAccountResponse'
            Lude.<$> (x Lude..?> "DedicatedTenancySupport")
            Lude.<*> (x Lude..?> "DedicatedTenancyManagementCidrRange")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAccount where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAccountResponse' smart constructor.
data DescribeAccountResponse = DescribeAccountResponse'
  { -- | The status of BYOL (whether BYOL is enabled or disabled).
    dedicatedTenancySupport :: Lude.Maybe DedicatedTenancySupportResultEnum,
    -- | The IP address range, specified as an IPv4 CIDR block, used for the management network interface.
    --
    -- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
    dedicatedTenancyManagementCidrRange :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountResponse' with the minimum fields required to make a request.
--
-- * 'dedicatedTenancySupport' - The status of BYOL (whether BYOL is enabled or disabled).
-- * 'dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, used for the management network interface.
--
-- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
-- * 'responseStatus' - The response status code.
mkDescribeAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountResponse
mkDescribeAccountResponse pResponseStatus_ =
  DescribeAccountResponse'
    { dedicatedTenancySupport = Lude.Nothing,
      dedicatedTenancyManagementCidrRange = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of BYOL (whether BYOL is enabled or disabled).
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsDedicatedTenancySupport :: Lens.Lens' DescribeAccountResponse (Lude.Maybe DedicatedTenancySupportResultEnum)
darsDedicatedTenancySupport = Lens.lens (dedicatedTenancySupport :: DescribeAccountResponse -> Lude.Maybe DedicatedTenancySupportResultEnum) (\s a -> s {dedicatedTenancySupport = a} :: DescribeAccountResponse)
{-# DEPRECATED darsDedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead." #-}

-- | The IP address range, specified as an IPv4 CIDR block, used for the management network interface.
--
-- The management network interface is connected to a secure Amazon WorkSpaces management network. It is used for interactive streaming of the WorkSpace desktop to Amazon WorkSpaces clients, and to allow Amazon WorkSpaces to manage the WorkSpace.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsDedicatedTenancyManagementCidrRange :: Lens.Lens' DescribeAccountResponse (Lude.Maybe Lude.Text)
darsDedicatedTenancyManagementCidrRange = Lens.lens (dedicatedTenancyManagementCidrRange :: DescribeAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {dedicatedTenancyManagementCidrRange = a} :: DescribeAccountResponse)
{-# DEPRECATED darsDedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAccountResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
