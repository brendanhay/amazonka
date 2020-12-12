{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGatewayManagementAPI.Types.Identity
  ( Identity (..),

    -- * Smart constructor
    mkIdentity,

    -- * Lenses
    iSourceIP,
    iUserAgent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkIdentity' smart constructor.
data Identity = Identity'
  { sourceIP :: Lude.Text,
    userAgent :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Identity' with the minimum fields required to make a request.
--
-- * 'sourceIP' - The source IP address of the TCP connection making the request to API Gateway.
-- * 'userAgent' - The User Agent of the API caller.
mkIdentity ::
  -- | 'sourceIP'
  Lude.Text ->
  -- | 'userAgent'
  Lude.Text ->
  Identity
mkIdentity pSourceIP_ pUserAgent_ =
  Identity' {sourceIP = pSourceIP_, userAgent = pUserAgent_}

-- | The source IP address of the TCP connection making the request to API Gateway.
--
-- /Note:/ Consider using 'sourceIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSourceIP :: Lens.Lens' Identity Lude.Text
iSourceIP = Lens.lens (sourceIP :: Identity -> Lude.Text) (\s a -> s {sourceIP = a} :: Identity)
{-# DEPRECATED iSourceIP "Use generic-lens or generic-optics with 'sourceIP' instead." #-}

-- | The User Agent of the API caller.
--
-- /Note:/ Consider using 'userAgent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iUserAgent :: Lens.Lens' Identity Lude.Text
iUserAgent = Lens.lens (userAgent :: Identity -> Lude.Text) (\s a -> s {userAgent = a} :: Identity)
{-# DEPRECATED iUserAgent "Use generic-lens or generic-optics with 'userAgent' instead." #-}

instance Lude.FromJSON Identity where
  parseJSON =
    Lude.withObject
      "Identity"
      ( \x ->
          Identity'
            Lude.<$> (x Lude..: "sourceIp") Lude.<*> (x Lude..: "userAgent")
      )
