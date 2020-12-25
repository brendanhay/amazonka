{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Redirect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Redirect
  ( Redirect (..),

    -- * Smart constructor
    mkRedirect,

    -- * Lenses
    rHostName,
    rHttpRedirectCode,
    rProtocol,
    rReplaceKeyPrefixWith,
    rReplaceKeyWith,
  )
where

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
  { -- | The host name to use in the redirect request.
    hostName :: Core.Maybe Types.HostName,
    -- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
    httpRedirectCode :: Core.Maybe Types.HttpRedirectCode,
    -- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
    protocol :: Core.Maybe Types.Protocol,
    -- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
    replaceKeyPrefixWith :: Core.Maybe Types.ReplaceKeyPrefixWith,
    -- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
    replaceKeyWith :: Core.Maybe Types.ReplaceKeyWith
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Redirect' value with any optional fields omitted.
mkRedirect ::
  Redirect
mkRedirect =
  Redirect'
    { hostName = Core.Nothing,
      httpRedirectCode = Core.Nothing,
      protocol = Core.Nothing,
      replaceKeyPrefixWith = Core.Nothing,
      replaceKeyWith = Core.Nothing
    }

-- | The host name to use in the redirect request.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHostName :: Lens.Lens' Redirect (Core.Maybe Types.HostName)
rHostName = Lens.field @"hostName"
{-# DEPRECATED rHostName "Use generic-lens or generic-optics with 'hostName' instead." #-}

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- /Note:/ Consider using 'httpRedirectCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHttpRedirectCode :: Lens.Lens' Redirect (Core.Maybe Types.HttpRedirectCode)
rHttpRedirectCode = Lens.field @"httpRedirectCode"
{-# DEPRECATED rHttpRedirectCode "Use generic-lens or generic-optics with 'httpRedirectCode' instead." #-}

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProtocol :: Lens.Lens' Redirect (Core.Maybe Types.Protocol)
rProtocol = Lens.field @"protocol"
{-# DEPRECATED rProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyPrefixWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyPrefixWith :: Lens.Lens' Redirect (Core.Maybe Types.ReplaceKeyPrefixWith)
rReplaceKeyPrefixWith = Lens.field @"replaceKeyPrefixWith"
{-# DEPRECATED rReplaceKeyPrefixWith "Use generic-lens or generic-optics with 'replaceKeyPrefixWith' instead." #-}

-- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyWith :: Lens.Lens' Redirect (Core.Maybe Types.ReplaceKeyWith)
rReplaceKeyWith = Lens.field @"replaceKeyWith"
{-# DEPRECATED rReplaceKeyWith "Use generic-lens or generic-optics with 'replaceKeyWith' instead." #-}

instance Core.ToXML Redirect where
  toXML Redirect {..} =
    Core.toXMLNode "HostName" Core.<$> hostName
      Core.<> Core.toXMLNode "HttpRedirectCode" Core.<$> httpRedirectCode
      Core.<> Core.toXMLNode "Protocol" Core.<$> protocol
      Core.<> Core.toXMLNode "ReplaceKeyPrefixWith" Core.<$> replaceKeyPrefixWith
      Core.<> Core.toXMLNode "ReplaceKeyWith" Core.<$> replaceKeyWith

instance Core.FromXML Redirect where
  parseXML x =
    Redirect'
      Core.<$> (x Core..@? "HostName")
      Core.<*> (x Core..@? "HttpRedirectCode")
      Core.<*> (x Core..@? "Protocol")
      Core.<*> (x Core..@? "ReplaceKeyPrefixWith")
      Core.<*> (x Core..@? "ReplaceKeyWith")
