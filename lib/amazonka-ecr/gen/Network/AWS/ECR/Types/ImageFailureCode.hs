{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailureCode
  ( ImageFailureCode
      ( ImageFailureCode',
        ImageFailureCodeInvalidImageDigest,
        ImageFailureCodeInvalidImageTag,
        ImageFailureCodeImageTagDoesNotMatchDigest,
        ImageFailureCodeImageNotFound,
        ImageFailureCodeMissingDigestAndTag,
        ImageFailureCodeImageReferencedByManifestList,
        ImageFailureCodeKmsError,
        fromImageFailureCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ImageFailureCode = ImageFailureCode'
  { fromImageFailureCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ImageFailureCodeInvalidImageDigest :: ImageFailureCode
pattern ImageFailureCodeInvalidImageDigest = ImageFailureCode' "InvalidImageDigest"

pattern ImageFailureCodeInvalidImageTag :: ImageFailureCode
pattern ImageFailureCodeInvalidImageTag = ImageFailureCode' "InvalidImageTag"

pattern ImageFailureCodeImageTagDoesNotMatchDigest :: ImageFailureCode
pattern ImageFailureCodeImageTagDoesNotMatchDigest = ImageFailureCode' "ImageTagDoesNotMatchDigest"

pattern ImageFailureCodeImageNotFound :: ImageFailureCode
pattern ImageFailureCodeImageNotFound = ImageFailureCode' "ImageNotFound"

pattern ImageFailureCodeMissingDigestAndTag :: ImageFailureCode
pattern ImageFailureCodeMissingDigestAndTag = ImageFailureCode' "MissingDigestAndTag"

pattern ImageFailureCodeImageReferencedByManifestList :: ImageFailureCode
pattern ImageFailureCodeImageReferencedByManifestList = ImageFailureCode' "ImageReferencedByManifestList"

pattern ImageFailureCodeKmsError :: ImageFailureCode
pattern ImageFailureCodeKmsError = ImageFailureCode' "KmsError"

{-# COMPLETE
  ImageFailureCodeInvalidImageDigest,
  ImageFailureCodeInvalidImageTag,
  ImageFailureCodeImageTagDoesNotMatchDigest,
  ImageFailureCodeImageNotFound,
  ImageFailureCodeMissingDigestAndTag,
  ImageFailureCodeImageReferencedByManifestList,
  ImageFailureCodeKmsError,
  ImageFailureCode'
  #-}
