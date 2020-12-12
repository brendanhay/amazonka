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
        ImageNotFound,
        ImageReferencedByManifestList,
        ImageTagDoesNotMatchDigest,
        InvalidImageDigest,
        InvalidImageTag,
        KMSError,
        MissingDigestAndTag
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageFailureCode = ImageFailureCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ImageNotFound :: ImageFailureCode
pattern ImageNotFound = ImageFailureCode' "ImageNotFound"

pattern ImageReferencedByManifestList :: ImageFailureCode
pattern ImageReferencedByManifestList = ImageFailureCode' "ImageReferencedByManifestList"

pattern ImageTagDoesNotMatchDigest :: ImageFailureCode
pattern ImageTagDoesNotMatchDigest = ImageFailureCode' "ImageTagDoesNotMatchDigest"

pattern InvalidImageDigest :: ImageFailureCode
pattern InvalidImageDigest = ImageFailureCode' "InvalidImageDigest"

pattern InvalidImageTag :: ImageFailureCode
pattern InvalidImageTag = ImageFailureCode' "InvalidImageTag"

pattern KMSError :: ImageFailureCode
pattern KMSError = ImageFailureCode' "KmsError"

pattern MissingDigestAndTag :: ImageFailureCode
pattern MissingDigestAndTag = ImageFailureCode' "MissingDigestAndTag"

{-# COMPLETE
  ImageNotFound,
  ImageReferencedByManifestList,
  ImageTagDoesNotMatchDigest,
  InvalidImageDigest,
  InvalidImageTag,
  KMSError,
  MissingDigestAndTag,
  ImageFailureCode'
  #-}
