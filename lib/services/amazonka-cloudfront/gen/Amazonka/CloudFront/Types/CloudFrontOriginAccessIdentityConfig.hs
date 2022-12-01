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
-- Module      : Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentityConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    callerReference :: Prelude.Text,
    -- | A comment to describe the origin access identity. The comment cannot be
    -- longer than 128 characters.
    comment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'comment', 'cloudFrontOriginAccessIdentityConfig_comment' - A comment to describe the origin access identity. The comment cannot be
-- longer than 128 characters.
newCloudFrontOriginAccessIdentityConfig ::
  -- | 'callerReference'
  Prelude.Text ->
  -- | 'comment'
  Prelude.Text ->
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
cloudFrontOriginAccessIdentityConfig_callerReference :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Prelude.Text
cloudFrontOriginAccessIdentityConfig_callerReference = Lens.lens (\CloudFrontOriginAccessIdentityConfig' {callerReference} -> callerReference) (\s@CloudFrontOriginAccessIdentityConfig' {} a -> s {callerReference = a} :: CloudFrontOriginAccessIdentityConfig)

-- | A comment to describe the origin access identity. The comment cannot be
-- longer than 128 characters.
cloudFrontOriginAccessIdentityConfig_comment :: Lens.Lens' CloudFrontOriginAccessIdentityConfig Prelude.Text
cloudFrontOriginAccessIdentityConfig_comment = Lens.lens (\CloudFrontOriginAccessIdentityConfig' {comment} -> comment) (\s@CloudFrontOriginAccessIdentityConfig' {} a -> s {comment = a} :: CloudFrontOriginAccessIdentityConfig)

instance
  Core.FromXML
    CloudFrontOriginAccessIdentityConfig
  where
  parseXML x =
    CloudFrontOriginAccessIdentityConfig'
      Prelude.<$> (x Core..@ "CallerReference")
      Prelude.<*> (x Core..@ "Comment")

instance
  Prelude.Hashable
    CloudFrontOriginAccessIdentityConfig
  where
  hashWithSalt
    _salt
    CloudFrontOriginAccessIdentityConfig' {..} =
      _salt `Prelude.hashWithSalt` callerReference
        `Prelude.hashWithSalt` comment

instance
  Prelude.NFData
    CloudFrontOriginAccessIdentityConfig
  where
  rnf CloudFrontOriginAccessIdentityConfig' {..} =
    Prelude.rnf callerReference
      `Prelude.seq` Prelude.rnf comment

instance
  Core.ToXML
    CloudFrontOriginAccessIdentityConfig
  where
  toXML CloudFrontOriginAccessIdentityConfig' {..} =
    Prelude.mconcat
      [ "CallerReference" Core.@= callerReference,
        "Comment" Core.@= comment
      ]
