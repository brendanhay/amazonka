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
    rProtocol,
    rHTTPRedirectCode,
    rReplaceKeyWith,
    rReplaceKeyPrefixWith,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Protocol

-- | Specifies how requests are redirected. In the event of an error, you can specify a different error code to return.
--
-- /See:/ 'mkRedirect' smart constructor.
data Redirect = Redirect'
  { -- | The host name to use in the redirect request.
    hostName :: Lude.Maybe Lude.Text,
    -- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
    protocol :: Lude.Maybe Protocol,
    -- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
    hTTPRedirectCode :: Lude.Maybe Lude.Text,
    -- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
    replaceKeyWith :: Lude.Maybe Lude.Text,
    -- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
    replaceKeyPrefixWith :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Redirect' with the minimum fields required to make a request.
--
-- * 'hostName' - The host name to use in the redirect request.
-- * 'protocol' - Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
-- * 'hTTPRedirectCode' - The HTTP redirect code to use on the response. Not required if one of the siblings is present.
-- * 'replaceKeyWith' - The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
-- * 'replaceKeyPrefixWith' - The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
mkRedirect ::
  Redirect
mkRedirect =
  Redirect'
    { hostName = Lude.Nothing,
      protocol = Lude.Nothing,
      hTTPRedirectCode = Lude.Nothing,
      replaceKeyWith = Lude.Nothing,
      replaceKeyPrefixWith = Lude.Nothing
    }

-- | The host name to use in the redirect request.
--
-- /Note:/ Consider using 'hostName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHostName :: Lens.Lens' Redirect (Lude.Maybe Lude.Text)
rHostName = Lens.lens (hostName :: Redirect -> Lude.Maybe Lude.Text) (\s a -> s {hostName = a} :: Redirect)
{-# DEPRECATED rHostName "Use generic-lens or generic-optics with 'hostName' instead." #-}

-- | Protocol to use when redirecting requests. The default is the protocol that is used in the original request.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rProtocol :: Lens.Lens' Redirect (Lude.Maybe Protocol)
rProtocol = Lens.lens (protocol :: Redirect -> Lude.Maybe Protocol) (\s a -> s {protocol = a} :: Redirect)
{-# DEPRECATED rProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The HTTP redirect code to use on the response. Not required if one of the siblings is present.
--
-- /Note:/ Consider using 'hTTPRedirectCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rHTTPRedirectCode :: Lens.Lens' Redirect (Lude.Maybe Lude.Text)
rHTTPRedirectCode = Lens.lens (hTTPRedirectCode :: Redirect -> Lude.Maybe Lude.Text) (\s a -> s {hTTPRedirectCode = a} :: Redirect)
{-# DEPRECATED rHTTPRedirectCode "Use generic-lens or generic-optics with 'hTTPRedirectCode' instead." #-}

-- | The specific object key to use in the redirect request. For example, redirect request to @error.html@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyPrefixWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyWith :: Lens.Lens' Redirect (Lude.Maybe Lude.Text)
rReplaceKeyWith = Lens.lens (replaceKeyWith :: Redirect -> Lude.Maybe Lude.Text) (\s a -> s {replaceKeyWith = a} :: Redirect)
{-# DEPRECATED rReplaceKeyWith "Use generic-lens or generic-optics with 'replaceKeyWith' instead." #-}

-- | The object key prefix to use in the redirect request. For example, to redirect requests for all pages with prefix @docs/@ (objects in the @docs/@ folder) to @documents/@ , you can set a condition block with @KeyPrefixEquals@ set to @docs/@ and in the Redirect set @ReplaceKeyPrefixWith@ to @/documents@ . Not required if one of the siblings is present. Can be present only if @ReplaceKeyWith@ is not provided.
--
-- /Note:/ Consider using 'replaceKeyPrefixWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplaceKeyPrefixWith :: Lens.Lens' Redirect (Lude.Maybe Lude.Text)
rReplaceKeyPrefixWith = Lens.lens (replaceKeyPrefixWith :: Redirect -> Lude.Maybe Lude.Text) (\s a -> s {replaceKeyPrefixWith = a} :: Redirect)
{-# DEPRECATED rReplaceKeyPrefixWith "Use generic-lens or generic-optics with 'replaceKeyPrefixWith' instead." #-}

instance Lude.FromXML Redirect where
  parseXML x =
    Redirect'
      Lude.<$> (x Lude..@? "HostName")
      Lude.<*> (x Lude..@? "Protocol")
      Lude.<*> (x Lude..@? "HttpRedirectCode")
      Lude.<*> (x Lude..@? "ReplaceKeyWith")
      Lude.<*> (x Lude..@? "ReplaceKeyPrefixWith")

instance Lude.ToXML Redirect where
  toXML Redirect' {..} =
    Lude.mconcat
      [ "HostName" Lude.@= hostName,
        "Protocol" Lude.@= protocol,
        "HttpRedirectCode" Lude.@= hTTPRedirectCode,
        "ReplaceKeyWith" Lude.@= replaceKeyWith,
        "ReplaceKeyPrefixWith" Lude.@= replaceKeyPrefixWith
      ]
