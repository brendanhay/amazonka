{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginCustomHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginCustomHeader
  ( OriginCustomHeader (..),

    -- * Smart constructor
    mkOriginCustomHeader,

    -- * Lenses
    ochHeaderValue,
    ochHeaderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains @HeaderName@ and @HeaderValue@ elements, if any, for this distribution.
--
-- /See:/ 'mkOriginCustomHeader' smart constructor.
data OriginCustomHeader = OriginCustomHeader'
  { -- | The value for the header that you specified in the @HeaderName@ field.
    headerValue :: Lude.Text,
    -- | The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
    headerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginCustomHeader' with the minimum fields required to make a request.
--
-- * 'headerValue' - The value for the header that you specified in the @HeaderName@ field.
-- * 'headerName' - The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
mkOriginCustomHeader ::
  -- | 'headerValue'
  Lude.Text ->
  -- | 'headerName'
  Lude.Text ->
  OriginCustomHeader
mkOriginCustomHeader pHeaderValue_ pHeaderName_ =
  OriginCustomHeader'
    { headerValue = pHeaderValue_,
      headerName = pHeaderName_
    }

-- | The value for the header that you specified in the @HeaderName@ field.
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ochHeaderValue :: Lens.Lens' OriginCustomHeader Lude.Text
ochHeaderValue = Lens.lens (headerValue :: OriginCustomHeader -> Lude.Text) (\s a -> s {headerValue = a} :: OriginCustomHeader)
{-# DEPRECATED ochHeaderValue "Use generic-lens or generic-optics with 'headerValue' instead." #-}

-- | The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ochHeaderName :: Lens.Lens' OriginCustomHeader Lude.Text
ochHeaderName = Lens.lens (headerName :: OriginCustomHeader -> Lude.Text) (\s a -> s {headerName = a} :: OriginCustomHeader)
{-# DEPRECATED ochHeaderName "Use generic-lens or generic-optics with 'headerName' instead." #-}

instance Lude.FromXML OriginCustomHeader where
  parseXML x =
    OriginCustomHeader'
      Lude.<$> (x Lude..@ "HeaderValue") Lude.<*> (x Lude..@ "HeaderName")

instance Lude.ToXML OriginCustomHeader where
  toXML OriginCustomHeader' {..} =
    Lude.mconcat
      [ "HeaderValue" Lude.@= headerValue,
        "HeaderName" Lude.@= headerName
      ]
