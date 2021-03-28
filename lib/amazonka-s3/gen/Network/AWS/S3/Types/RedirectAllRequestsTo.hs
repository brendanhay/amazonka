{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RedirectAllRequestsTo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.RedirectAllRequestsTo
  ( RedirectAllRequestsTo (..)
  -- * Smart constructor
  , mkRedirectAllRequestsTo
  -- * Lenses
  , rartHostName
  , rartProtocol
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.HostName as Types
import qualified Network.AWS.S3.Types.Protocol as Types

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
-- /See:/ 'mkRedirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { hostName :: Types.HostName
    -- ^ Name of the host where requests are redirected.
  , protocol :: Core.Maybe Types.Protocol
    -- ^ Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedirectAllRequestsTo' value with any optional fields omitted.
mkRedirectAllRequestsTo
    :: Types.HostName -- ^ 'hostName'
    -> RedirectAllRequestsTo
mkRedirectAllRequestsTo hostName
  = RedirectAllRequestsTo'{hostName, protocol = Core.Nothing}

-- | Name of the host where requests are redirected.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rartHostName :: Lens.Lens' RedirectAllRequestsTo Types.HostName
rartHostName = Lens.field @"hostName"
{-# INLINEABLE rartHostName #-}
{-# DEPRECATED hostName "Use generic-lens or generic-optics with 'hostName' instead"  #-}

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rartProtocol :: Lens.Lens' RedirectAllRequestsTo (Core.Maybe Types.Protocol)
rartProtocol = Lens.field @"protocol"
{-# INLINEABLE rartProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

instance Core.ToXML RedirectAllRequestsTo where
        toXML RedirectAllRequestsTo{..}
          = Core.toXMLElement "HostName" hostName Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Protocol") protocol

instance Core.FromXML RedirectAllRequestsTo where
        parseXML x
          = RedirectAllRequestsTo' Core.<$>
              (x Core..@ "HostName") Core.<*> x Core..@? "Protocol"
