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
    cfoaicComment,
    cfoaicCallerReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Origin access identity configuration. Send a @GET@ request to the @//CloudFront API version/ /CloudFront/identity ID/config@ resource.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
  { -- | Any comments you want to include about the origin access identity.
    comment :: Lude.Text,
    -- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created.
    -- If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.
    -- If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFrontOriginAccessIdentityConfig' with the minimum fields required to make a request.
--
-- * 'comment' - Any comments you want to include about the origin access identity.
-- * 'callerReference' - A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created.
-- If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.
-- If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
mkCloudFrontOriginAccessIdentityConfig ::
  -- | 'comment'
  Lude.Text ->
  -- | 'callerReference'
  Lude.Text ->
  CloudFrontOriginAccessIdentityConfig
mkCloudFrontOriginAccessIdentityConfig pComment_ pCallerReference_ =
  CloudFrontOriginAccessIdentityConfig'
    { comment = pComment_,
      callerReference = pCallerReference_
    }

-- | Any comments you want to include about the origin access identity.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaicComment :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Lude.Text
cfoaicComment = Lens.lens (comment :: CloudFrontOriginAccessIdentityConfig -> Lude.Text) (\s a -> s {comment = a} :: CloudFrontOriginAccessIdentityConfig)
{-# DEPRECATED cfoaicComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A unique value (for example, a date-time stamp) that ensures that the request can't be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access identity is created.
-- If the @CallerReference@ is a value already sent in a previous identity request, and the content of the @CloudFrontOriginAccessIdentityConfig@ is identical to the original request (ignoring white space), the response includes the same information returned to the original request.
-- If the @CallerReference@ is a value you already sent in a previous request to create an identity, but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaicCallerReference :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Lude.Text
cfoaicCallerReference = Lens.lens (callerReference :: CloudFrontOriginAccessIdentityConfig -> Lude.Text) (\s a -> s {callerReference = a} :: CloudFrontOriginAccessIdentityConfig)
{-# DEPRECATED cfoaicCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML CloudFrontOriginAccessIdentityConfig where
  parseXML x =
    CloudFrontOriginAccessIdentityConfig'
      Lude.<$> (x Lude..@ "Comment") Lude.<*> (x Lude..@ "CallerReference")

instance Lude.ToXML CloudFrontOriginAccessIdentityConfig where
  toXML CloudFrontOriginAccessIdentityConfig' {..} =
    Lude.mconcat
      [ "Comment" Lude.@= comment,
        "CallerReference" Lude.@= callerReference
      ]
