{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Origin access identity configuration. Send a @GET@ request to the
-- @\/CloudFront API version\/CloudFront\/identity ID\/config@ resource.
--
-- /See:/ 'newCloudFrontOriginAccessIdentityConfig' smart constructor.
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
  { -- | A unique value (for example, a date-time stamp) that ensures that the
    -- request can\'t be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of
    -- the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access
    -- identity is created.
    --
    -- If the @CallerReference@ is a value already sent in a previous identity
    -- request, and the content of the @CloudFrontOriginAccessIdentityConfig@
    -- is identical to the original request (ignoring white space), the
    -- response includes the same information returned to the original request.
    --
    -- If the @CallerReference@ is a value you already sent in a previous
    -- request to create an identity, but the content of the
    -- @CloudFrontOriginAccessIdentityConfig@ is different from the original
    -- request, CloudFront returns a
    -- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
    callerReference :: Core.Text,
    -- | Any comments you want to include about the origin access identity.
    comment :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudFrontOriginAccessIdentityConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callerReference', 'cloudFrontOriginAccessIdentityConfig_callerReference' - A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access
-- identity is created.
--
-- If the @CallerReference@ is a value already sent in a previous identity
-- request, and the content of the @CloudFrontOriginAccessIdentityConfig@
-- is identical to the original request (ignoring white space), the
-- response includes the same information returned to the original request.
--
-- If the @CallerReference@ is a value you already sent in a previous
-- request to create an identity, but the content of the
-- @CloudFrontOriginAccessIdentityConfig@ is different from the original
-- request, CloudFront returns a
-- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
--
-- 'comment', 'cloudFrontOriginAccessIdentityConfig_comment' - Any comments you want to include about the origin access identity.
newCloudFrontOriginAccessIdentityConfig ::
  -- | 'callerReference'
  Core.Text ->
  -- | 'comment'
  Core.Text ->
  CloudFrontOriginAccessIdentityConfig
newCloudFrontOriginAccessIdentityConfig
  pCallerReference_
  pComment_ =
    CloudFrontOriginAccessIdentityConfig'
      { callerReference =
          pCallerReference_,
        comment = pComment_
      }

-- | A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @CloudFrontOriginAccessIdentityConfig@ object), a new origin access
-- identity is created.
--
-- If the @CallerReference@ is a value already sent in a previous identity
-- request, and the content of the @CloudFrontOriginAccessIdentityConfig@
-- is identical to the original request (ignoring white space), the
-- response includes the same information returned to the original request.
--
-- If the @CallerReference@ is a value you already sent in a previous
-- request to create an identity, but the content of the
-- @CloudFrontOriginAccessIdentityConfig@ is different from the original
-- request, CloudFront returns a
-- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
cloudFrontOriginAccessIdentityConfig_callerReference :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Core.Text
cloudFrontOriginAccessIdentityConfig_callerReference = Lens.lens (\CloudFrontOriginAccessIdentityConfig' {callerReference} -> callerReference) (\s@CloudFrontOriginAccessIdentityConfig' {} a -> s {callerReference = a} :: CloudFrontOriginAccessIdentityConfig)

-- | Any comments you want to include about the origin access identity.
cloudFrontOriginAccessIdentityConfig_comment :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Core.Text
cloudFrontOriginAccessIdentityConfig_comment = Lens.lens (\CloudFrontOriginAccessIdentityConfig' {comment} -> comment) (\s@CloudFrontOriginAccessIdentityConfig' {} a -> s {comment = a} :: CloudFrontOriginAccessIdentityConfig)

instance
  Core.FromXML
    CloudFrontOriginAccessIdentityConfig
  where
  parseXML x =
    CloudFrontOriginAccessIdentityConfig'
      Core.<$> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "Comment")

instance
  Core.Hashable
    CloudFrontOriginAccessIdentityConfig

instance
  Core.NFData
    CloudFrontOriginAccessIdentityConfig

instance
  Core.ToXML
    CloudFrontOriginAccessIdentityConfig
  where
  toXML CloudFrontOriginAccessIdentityConfig' {..} =
    Core.mconcat
      [ "CallerReference" Core.@= callerReference,
        "Comment" Core.@= comment
      ]
