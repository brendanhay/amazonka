{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
  ( CloudFrontOriginAccessIdentityConfig (..),

    -- * Smart constructor
    mkCloudFrontOriginAccessIdentityConfig,

    -- * Lenses
    cfoaicCallerReference,
    cfoaicComment,
  )
where

import qualified Network.AWS.CloudFront.Types.CallerReference as Types
import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Origin access identity configuration. Send a @GET@ request to the @//CloudFront API version/ /CloudFront/identity ID/config@ resource.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
  { -- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created.
    -- If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.
    -- If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
    callerReference :: Types.CallerReference,
    -- | Any comments you want to include about the origin access identity.
    comment :: Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFrontOriginAccessIdentityConfig' value with any optional fields omitted.
mkCloudFrontOriginAccessIdentityConfig ::
  -- | 'callerReference'
  Types.CallerReference ->
  -- | 'comment'
  Types.Comment ->
  CloudFrontOriginAccessIdentityConfig
mkCloudFrontOriginAccessIdentityConfig callerReference comment =
  CloudFrontOriginAccessIdentityConfig' {callerReference, comment}

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created.
-- If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.
-- If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaicCallerReference :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Types.CallerReference
cfoaicCallerReference = Lens.field @"callerReference"
{-# DEPRECATED cfoaicCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | Any comments you want to include about the origin access identity.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaicComment :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Types.Comment
cfoaicComment = Lens.field @"comment"
{-# DEPRECATED cfoaicComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.ToXML CloudFrontOriginAccessIdentityConfig where
  toXML CloudFrontOriginAccessIdentityConfig {..} =
    Core.toXMLNode "CallerReference" callerReference
      Core.<> Core.toXMLNode "Comment" comment

instance Core.FromXML CloudFrontOriginAccessIdentityConfig where
  parseXML x =
    CloudFrontOriginAccessIdentityConfig'
      Core.<$> (x Core..@ "CallerReference") Core.<*> (x Core..@ "Comment")
