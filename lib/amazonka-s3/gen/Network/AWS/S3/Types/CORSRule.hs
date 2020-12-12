{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CORSRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CORSRule
  ( CORSRule (..),

    -- * Smart constructor
    mkCORSRule,

    -- * Lenses
    crMaxAgeSeconds,
    crAllowedHeaders,
    crExposeHeaders,
    crAllowedMethods,
    crAllowedOrigins,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies a cross-origin access rule for an Amazon S3 bucket.
--
-- /See:/ 'mkCORSRule' smart constructor.
data CORSRule = CORSRule'
  { maxAgeSeconds :: Lude.Maybe Lude.Int,
    allowedHeaders :: Lude.Maybe [Lude.Text],
    exposeHeaders :: Lude.Maybe [Lude.Text],
    allowedMethods :: [Lude.Text],
    allowedOrigins :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- * 'allowedHeaders' - Headers that are specified in the @Access-Control-Request-Headers@ header. These headers are allowed in a preflight OPTIONS request. In response to any preflight OPTIONS request, Amazon S3 returns any requested headers that are allowed.
-- * 'allowedMethods' - An HTTP method that you allow the origin to execute. Valid values are @GET@ , @PUT@ , @HEAD@ , @POST@ , and @DELETE@ .
-- * 'allowedOrigins' - One or more origins you want customers to be able to access the bucket from.
-- * 'exposeHeaders' - One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
-- * 'maxAgeSeconds' - The time in seconds that your browser is to cache the preflight response for the specified resource.
mkCORSRule ::
  CORSRule
mkCORSRule =
  CORSRule'
    { maxAgeSeconds = Lude.Nothing,
      allowedHeaders = Lude.Nothing,
      exposeHeaders = Lude.Nothing,
      allowedMethods = Lude.mempty,
      allowedOrigins = Lude.mempty
    }

-- | The time in seconds that your browser is to cache the preflight response for the specified resource.
--
-- /Note:/ Consider using 'maxAgeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxAgeSeconds :: Lens.Lens' CORSRule (Lude.Maybe Lude.Int)
crMaxAgeSeconds = Lens.lens (maxAgeSeconds :: CORSRule -> Lude.Maybe Lude.Int) (\s a -> s {maxAgeSeconds = a} :: CORSRule)
{-# DEPRECATED crMaxAgeSeconds "Use generic-lens or generic-optics with 'maxAgeSeconds' instead." #-}

-- | Headers that are specified in the @Access-Control-Request-Headers@ header. These headers are allowed in a preflight OPTIONS request. In response to any preflight OPTIONS request, Amazon S3 returns any requested headers that are allowed.
--
-- /Note:/ Consider using 'allowedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedHeaders :: Lens.Lens' CORSRule (Lude.Maybe [Lude.Text])
crAllowedHeaders = Lens.lens (allowedHeaders :: CORSRule -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedHeaders = a} :: CORSRule)
{-# DEPRECATED crAllowedHeaders "Use generic-lens or generic-optics with 'allowedHeaders' instead." #-}

-- | One or more headers in the response that you want customers to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- /Note:/ Consider using 'exposeHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crExposeHeaders :: Lens.Lens' CORSRule (Lude.Maybe [Lude.Text])
crExposeHeaders = Lens.lens (exposeHeaders :: CORSRule -> Lude.Maybe [Lude.Text]) (\s a -> s {exposeHeaders = a} :: CORSRule)
{-# DEPRECATED crExposeHeaders "Use generic-lens or generic-optics with 'exposeHeaders' instead." #-}

-- | An HTTP method that you allow the origin to execute. Valid values are @GET@ , @PUT@ , @HEAD@ , @POST@ , and @DELETE@ .
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedMethods :: Lens.Lens' CORSRule [Lude.Text]
crAllowedMethods = Lens.lens (allowedMethods :: CORSRule -> [Lude.Text]) (\s a -> s {allowedMethods = a} :: CORSRule)
{-# DEPRECATED crAllowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead." #-}

-- | One or more origins you want customers to be able to access the bucket from.
--
-- /Note:/ Consider using 'allowedOrigins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedOrigins :: Lens.Lens' CORSRule [Lude.Text]
crAllowedOrigins = Lens.lens (allowedOrigins :: CORSRule -> [Lude.Text]) (\s a -> s {allowedOrigins = a} :: CORSRule)
{-# DEPRECATED crAllowedOrigins "Use generic-lens or generic-optics with 'allowedOrigins' instead." #-}

instance Lude.FromXML CORSRule where
  parseXML x =
    CORSRule'
      Lude.<$> (x Lude..@? "MaxAgeSeconds")
      Lude.<*> (Lude.may (Lude.parseXMLList "AllowedHeader") x)
      Lude.<*> (Lude.may (Lude.parseXMLList "ExposeHeader") x)
      Lude.<*> (Lude.parseXMLList "AllowedMethod" x)
      Lude.<*> (Lude.parseXMLList "AllowedOrigin" x)

instance Lude.ToXML CORSRule where
  toXML CORSRule' {..} =
    Lude.mconcat
      [ "MaxAgeSeconds" Lude.@= maxAgeSeconds,
        Lude.toXML
          (Lude.toXMLList "AllowedHeader" Lude.<$> allowedHeaders),
        Lude.toXML (Lude.toXMLList "ExposeHeader" Lude.<$> exposeHeaders),
        Lude.toXMLList "AllowedMethod" allowedMethods,
        Lude.toXMLList "AllowedOrigin" allowedOrigins
      ]
