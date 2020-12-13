{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RedirectAllRequestsTo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RedirectAllRequestsTo
  ( RedirectAllRequestsTo (..),

    -- * Smart constructor
    mkRedirectAllRequestsTo,

    -- * Lenses
    rartHostName,
    rartProtocol,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Protocol

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
-- /See:/ 'mkRedirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { -- | Name of the host where requests are redirected.
    hostName :: Lude.Text,
    -- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
    protocol :: Lude.Maybe Protocol
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedirectAllRequestsTo' with the minimum fields required to make a request.
--
-- * 'hostName' - Name of the host where requests are redirected.
-- * 'protocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
mkRedirectAllRequestsTo ::
  -- | 'hostName'
  Lude.Text ->
  RedirectAllRequestsTo
mkRedirectAllRequestsTo pHostName_ =
  RedirectAllRequestsTo'
    { hostName = pHostName_,
      protocol = Lude.Nothing
    }

-- | Name of the host where requests are redirected.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rartHostName :: Lens.Lens' RedirectAllRequestsTo Lude.Text
rartHostName = Lens.lens (hostName :: RedirectAllRequestsTo -> Lude.Text) (\s a -> s {hostName = a} :: RedirectAllRequestsTo)
{-# DEPRECATED rartHostName "Use generic-lens or generic-optics with 'hostName' instead." #-}

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rartProtocol :: Lens.Lens' RedirectAllRequestsTo (Lude.Maybe Protocol)
rartProtocol = Lens.lens (protocol :: RedirectAllRequestsTo -> Lude.Maybe Protocol) (\s a -> s {protocol = a} :: RedirectAllRequestsTo)
{-# DEPRECATED rartProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

instance Lude.FromXML RedirectAllRequestsTo where
  parseXML x =
    RedirectAllRequestsTo'
      Lude.<$> (x Lude..@ "HostName") Lude.<*> (x Lude..@? "Protocol")

instance Lude.ToXML RedirectAllRequestsTo where
  toXML RedirectAllRequestsTo' {..} =
    Lude.mconcat
      ["HostName" Lude.@= hostName, "Protocol" Lude.@= protocol]
