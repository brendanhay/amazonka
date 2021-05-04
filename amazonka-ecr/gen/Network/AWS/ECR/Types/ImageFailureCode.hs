{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailureCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailureCode
  ( ImageFailureCode
      ( ..,
        ImageFailureCode_ImageNotFound,
        ImageFailureCode_ImageReferencedByManifestList,
        ImageFailureCode_ImageTagDoesNotMatchDigest,
        ImageFailureCode_InvalidImageDigest,
        ImageFailureCode_InvalidImageTag,
        ImageFailureCode_KmsError,
        ImageFailureCode_MissingDigestAndTag
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ImageFailureCode = ImageFailureCode'
  { fromImageFailureCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ImageFailureCode_ImageNotFound :: ImageFailureCode
pattern ImageFailureCode_ImageNotFound = ImageFailureCode' "ImageNotFound"

pattern ImageFailureCode_ImageReferencedByManifestList :: ImageFailureCode
pattern ImageFailureCode_ImageReferencedByManifestList = ImageFailureCode' "ImageReferencedByManifestList"

pattern ImageFailureCode_ImageTagDoesNotMatchDigest :: ImageFailureCode
pattern ImageFailureCode_ImageTagDoesNotMatchDigest = ImageFailureCode' "ImageTagDoesNotMatchDigest"

pattern ImageFailureCode_InvalidImageDigest :: ImageFailureCode
pattern ImageFailureCode_InvalidImageDigest = ImageFailureCode' "InvalidImageDigest"

pattern ImageFailureCode_InvalidImageTag :: ImageFailureCode
pattern ImageFailureCode_InvalidImageTag = ImageFailureCode' "InvalidImageTag"

pattern ImageFailureCode_KmsError :: ImageFailureCode
pattern ImageFailureCode_KmsError = ImageFailureCode' "KmsError"

pattern ImageFailureCode_MissingDigestAndTag :: ImageFailureCode
pattern ImageFailureCode_MissingDigestAndTag = ImageFailureCode' "MissingDigestAndTag"

{-# COMPLETE
  ImageFailureCode_ImageNotFound,
  ImageFailureCode_ImageReferencedByManifestList,
  ImageFailureCode_ImageTagDoesNotMatchDigest,
  ImageFailureCode_InvalidImageDigest,
  ImageFailureCode_InvalidImageTag,
  ImageFailureCode_KmsError,
  ImageFailureCode_MissingDigestAndTag,
  ImageFailureCode'
  #-}
