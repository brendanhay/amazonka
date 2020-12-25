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
    ochHeaderName,
    ochHeaderValue,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains @HeaderName@ and @HeaderValue@ elements, if any, for this distribution.
--
-- /See:/ 'mkOriginCustomHeader' smart constructor.
data OriginCustomHeader = OriginCustomHeader'
  { -- | The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
    headerName :: Types.String,
    -- | The value for the header that you specified in the @HeaderName@ field.
    headerValue :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginCustomHeader' value with any optional fields omitted.
mkOriginCustomHeader ::
  -- | 'headerName'
  Types.String ->
  -- | 'headerValue'
  Types.String ->
  OriginCustomHeader
mkOriginCustomHeader headerName headerValue =
  OriginCustomHeader' {headerName, headerValue}

-- | The name of a header that you want CloudFront to send to your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/forward-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'headerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ochHeaderName :: Lens.Lens' OriginCustomHeader Types.String
ochHeaderName = Lens.field @"headerName"
{-# DEPRECATED ochHeaderName "Use generic-lens or generic-optics with 'headerName' instead." #-}

-- | The value for the header that you specified in the @HeaderName@ field.
--
-- /Note:/ Consider using 'headerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ochHeaderValue :: Lens.Lens' OriginCustomHeader Types.String
ochHeaderValue = Lens.field @"headerValue"
{-# DEPRECATED ochHeaderValue "Use generic-lens or generic-optics with 'headerValue' instead." #-}

instance Core.ToXML OriginCustomHeader where
  toXML OriginCustomHeader {..} =
    Core.toXMLNode "HeaderName" headerName
      Core.<> Core.toXMLNode "HeaderValue" headerValue

instance Core.FromXML OriginCustomHeader where
  parseXML x =
    OriginCustomHeader'
      Core.<$> (x Core..@ "HeaderName") Core.<*> (x Core..@ "HeaderValue")
