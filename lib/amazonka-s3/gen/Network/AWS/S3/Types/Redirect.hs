{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Redirect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Redirect
  ( Redirect (..)
  -- * Smart constructor
  , mkRedirect
  -- * Lenses
  , rHostName
  , rHttpRedirectCode
  , rProtocol
  , rReplaceKeyPrefixWith
  , rReplaceKeyWith
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.HostName as Types
import qualified Network.AWS.S3.Types.HttpRedirectCode as Types
import qualified Network.AWS.S3.Types.Protocol as Types
import qualified Network.AWS.S3.Types.ReplaceKeyPrefixWith as Types
import qualified Network.AWS.S3.Types.ReplaceKeyWith as Types

-- | Specifies how requests are redirected. In the event of an error, you can specify a different error code to return.
--
-- /See:/ 'mkRedirect' smart constructor.
data Redirect = Redirect'
  { hostName :: Core.Maybe Types.HostName
    -- ^ The host name to use in the redirect request.
  , httpRedirectCode :: Core.Maybe Types.HttpRedirectCode
    -- ^ The HTTP redirect code to use on the response. Not required if one of the siblings is present.
  , protocol :: Core.Maybe Types.Protocol
    -- ^ Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
  , replaceKeyPrefixWith :: Core.Maybe Types.ReplaceKeyPrefixWith
    -- ^ The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
  , replaceKeyWith :: Core.Maybe Types.ReplaceKeyWith
    -- ^ The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Redirect' value with any optional fields omitted.
mkRedirect
    :: Redirect
mkRedirect
  = Redirect'{hostName = Core.Nothing,
              httpRedirectCode = Core.Nothing, protocol = Core.Nothing,
              replaceKeyPrefixWith = Core.Nothing, replaceKeyWith = Core.Nothing}

-- | The host name to use in the redirect request.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHostName :: Lens.Lens' Redirect (Core.Maybe Types.HostName)
rHostName = Lens.field @"hostName"
{-# INLINEABLE rHostName #-}
{-# DEPRECATED hostName "Use generic-lens or generic-optics with 'hostName' instead"  #-}

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- /Note:/ Consider using 'httpRedirectCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHttpRedirectCode :: Lens.Lens' Redirect (Core.Maybe Types.HttpRedirectCode)
rHttpRedirectCode = Lens.field @"httpRedirectCode"
{-# INLINEABLE rHttpRedirectCode #-}
{-# DEPRECATED httpRedirectCode "Use generic-lens or generic-optics with 'httpRedirectCode' instead"  #-}

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProtocol :: Lens.Lens' Redirect (Core.Maybe Types.Protocol)
rProtocol = Lens.field @"protocol"
{-# INLINEABLE rProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyPrefixWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyPrefixWith :: Lens.Lens' Redirect (Core.Maybe Types.ReplaceKeyPrefixWith)
rReplaceKeyPrefixWith = Lens.field @"replaceKeyPrefixWith"
{-# INLINEABLE rReplaceKeyPrefixWith #-}
{-# DEPRECATED replaceKeyPrefixWith "Use generic-lens or generic-optics with 'replaceKeyPrefixWith' instead"  #-}

-- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyWith :: Lens.Lens' Redirect (Core.Maybe Types.ReplaceKeyWith)
rReplaceKeyWith = Lens.field @"replaceKeyWith"
{-# INLINEABLE rReplaceKeyWith #-}
{-# DEPRECATED replaceKeyWith "Use generic-lens or generic-optics with 'replaceKeyWith' instead"  #-}

instance Core.ToXML Redirect where
        toXML Redirect{..}
          = Core.maybe Core.mempty (Core.toXMLElement "HostName") hostName
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "HttpRedirectCode")
                httpRedirectCode
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Protocol") protocol
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ReplaceKeyPrefixWith")
                replaceKeyPrefixWith
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "ReplaceKeyWith")
                replaceKeyWith

instance Core.FromXML Redirect where
        parseXML x
          = Redirect' Core.<$>
              (x Core..@? "HostName") Core.<*> x Core..@? "HttpRedirectCode"
                Core.<*> x Core..@? "Protocol"
                Core.<*> x Core..@? "ReplaceKeyPrefixWith"
                Core.<*> x Core..@? "ReplaceKeyWith"
