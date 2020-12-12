{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.HTTP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.HTTP
  ( HTTP (..),

    -- * Smart constructor
    mkHTTP,

    -- * Lenses
    httpHTTPMethod,
    httpHTTPStatus,
    httpClientIP,
    httpUserAgent,
    httpHTTPURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an HTTP request.
--
-- /See:/ 'mkHTTP' smart constructor.
data HTTP = HTTP'
  { hTTPMethod :: Lude.Maybe Lude.Text,
    httpstatus :: Lude.Maybe Lude.Int,
    clientIP :: Lude.Maybe Lude.Text,
    userAgent :: Lude.Maybe Lude.Text,
    hTTPURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTP' with the minimum fields required to make a request.
--
-- * 'clientIP' - The IP address of the requestor.
-- * 'hTTPMethod' - The request method.
-- * 'hTTPURL' - The request URL.
-- * 'httpstatus' - The response status.
-- * 'userAgent' - The request's user agent string.
mkHTTP ::
  HTTP
mkHTTP =
  HTTP'
    { hTTPMethod = Lude.Nothing,
      httpstatus = Lude.Nothing,
      clientIP = Lude.Nothing,
      userAgent = Lude.Nothing,
      hTTPURL = Lude.Nothing
    }

-- | The request method.
--
-- /Note:/ Consider using 'hTTPMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpHTTPMethod :: Lens.Lens' HTTP (Lude.Maybe Lude.Text)
httpHTTPMethod = Lens.lens (hTTPMethod :: HTTP -> Lude.Maybe Lude.Text) (\s a -> s {hTTPMethod = a} :: HTTP)
{-# DEPRECATED httpHTTPMethod "Use generic-lens or generic-optics with 'hTTPMethod' instead." #-}

-- | The response status.
--
-- /Note:/ Consider using 'httpstatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpHTTPStatus :: Lens.Lens' HTTP (Lude.Maybe Lude.Int)
httpHTTPStatus = Lens.lens (httpstatus :: HTTP -> Lude.Maybe Lude.Int) (\s a -> s {httpstatus = a} :: HTTP)
{-# DEPRECATED httpHTTPStatus "Use generic-lens or generic-optics with 'httpstatus' instead." #-}

-- | The IP address of the requestor.
--
-- /Note:/ Consider using 'clientIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpClientIP :: Lens.Lens' HTTP (Lude.Maybe Lude.Text)
httpClientIP = Lens.lens (clientIP :: HTTP -> Lude.Maybe Lude.Text) (\s a -> s {clientIP = a} :: HTTP)
{-# DEPRECATED httpClientIP "Use generic-lens or generic-optics with 'clientIP' instead." #-}

-- | The request's user agent string.
--
-- /Note:/ Consider using 'userAgent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpUserAgent :: Lens.Lens' HTTP (Lude.Maybe Lude.Text)
httpUserAgent = Lens.lens (userAgent :: HTTP -> Lude.Maybe Lude.Text) (\s a -> s {userAgent = a} :: HTTP)
{-# DEPRECATED httpUserAgent "Use generic-lens or generic-optics with 'userAgent' instead." #-}

-- | The request URL.
--
-- /Note:/ Consider using 'hTTPURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpHTTPURL :: Lens.Lens' HTTP (Lude.Maybe Lude.Text)
httpHTTPURL = Lens.lens (hTTPURL :: HTTP -> Lude.Maybe Lude.Text) (\s a -> s {hTTPURL = a} :: HTTP)
{-# DEPRECATED httpHTTPURL "Use generic-lens or generic-optics with 'hTTPURL' instead." #-}

instance Lude.FromJSON HTTP where
  parseJSON =
    Lude.withObject
      "HTTP"
      ( \x ->
          HTTP'
            Lude.<$> (x Lude..:? "HttpMethod")
            Lude.<*> (x Lude..:? "HttpStatus")
            Lude.<*> (x Lude..:? "ClientIp")
            Lude.<*> (x Lude..:? "UserAgent")
            Lude.<*> (x Lude..:? "HttpURL")
      )
