{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of Bring Your Own License (BYOL) for the specified account.
module Network.AWS.WorkSpaces.ModifyAccount
  ( -- * Creating a request
    ModifyAccount (..),
    mkModifyAccount,

    -- ** Request lenses
    maDedicatedTenancySupport,
    maDedicatedTenancyManagementCidrRange,

    -- * Destructuring the response
    ModifyAccountResponse (..),
    mkModifyAccountResponse,

    -- ** Response lenses
    marsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkModifyAccount' smart constructor.
data ModifyAccount = ModifyAccount'
  { dedicatedTenancySupport ::
      Lude.Maybe DedicatedTenancySupportEnum,
    dedicatedTenancyManagementCidrRange :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyAccount' with the minimum fields required to make a request.
--
-- * 'dedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
-- * 'dedicatedTenancySupport' - The status of BYOL.
mkModifyAccount ::
  ModifyAccount
mkModifyAccount =
  ModifyAccount'
    { dedicatedTenancySupport = Lude.Nothing,
      dedicatedTenancyManagementCidrRange = Lude.Nothing
    }

-- | The status of BYOL.
--
-- /Note:/ Consider using 'dedicatedTenancySupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDedicatedTenancySupport :: Lens.Lens' ModifyAccount (Lude.Maybe DedicatedTenancySupportEnum)
maDedicatedTenancySupport = Lens.lens (dedicatedTenancySupport :: ModifyAccount -> Lude.Maybe DedicatedTenancySupportEnum) (\s a -> s {dedicatedTenancySupport = a} :: ModifyAccount)
{-# DEPRECATED maDedicatedTenancySupport "Use generic-lens or generic-optics with 'dedicatedTenancySupport' instead." #-}

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface. Specify an IP address range that is compatible with your network and in CIDR notation (that is, specify the range as an IPv4 CIDR block). The CIDR block size must be /16 (for example, 203.0.113.25/16). It must also be specified as available by the @ListAvailableManagementCidrRanges@ operation.
--
-- /Note:/ Consider using 'dedicatedTenancyManagementCidrRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maDedicatedTenancyManagementCidrRange :: Lens.Lens' ModifyAccount (Lude.Maybe Lude.Text)
maDedicatedTenancyManagementCidrRange = Lens.lens (dedicatedTenancyManagementCidrRange :: ModifyAccount -> Lude.Maybe Lude.Text) (\s a -> s {dedicatedTenancyManagementCidrRange = a} :: ModifyAccount)
{-# DEPRECATED maDedicatedTenancyManagementCidrRange "Use generic-lens or generic-optics with 'dedicatedTenancyManagementCidrRange' instead." #-}

instance Lude.AWSRequest ModifyAccount where
  type Rs ModifyAccount = ModifyAccountResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyAccountResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.ModifyAccount" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyAccount where
  toJSON ModifyAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DedicatedTenancySupport" Lude..=)
              Lude.<$> dedicatedTenancySupport,
            ("DedicatedTenancyManagementCidrRange" Lude..=)
              Lude.<$> dedicatedTenancyManagementCidrRange
          ]
      )

instance Lude.ToPath ModifyAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyAccountResponse' smart constructor.
newtype ModifyAccountResponse = ModifyAccountResponse'
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

-- | Creates a value of 'ModifyAccountResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyAccountResponse
mkModifyAccountResponse pResponseStatus_ =
  ModifyAccountResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marsResponseStatus :: Lens.Lens' ModifyAccountResponse Lude.Int
marsResponseStatus = Lens.lens (responseStatus :: ModifyAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyAccountResponse)
{-# DEPRECATED marsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
