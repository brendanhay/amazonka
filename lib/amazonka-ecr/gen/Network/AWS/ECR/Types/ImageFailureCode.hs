{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailureCode where

import Network.AWS.Prelude

data ImageFailureCode
  = ImageNotFound
  | ImageReferencedByManifestList
  | ImageTagDoesNotMatchDigest
  | InvalidImageDigest
  | InvalidImageTag
  | KMSError
  | MissingDigestAndTag
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ImageFailureCode where
  parser =
    takeLowerText >>= \case
      "imagenotfound" -> pure ImageNotFound
      "imagereferencedbymanifestlist" -> pure ImageReferencedByManifestList
      "imagetagdoesnotmatchdigest" -> pure ImageTagDoesNotMatchDigest
      "invalidimagedigest" -> pure InvalidImageDigest
      "invalidimagetag" -> pure InvalidImageTag
      "kmserror" -> pure KMSError
      "missingdigestandtag" -> pure MissingDigestAndTag
      e ->
        fromTextError $
          "Failure parsing ImageFailureCode from value: '" <> e
            <> "'. Accepted values: imagenotfound, imagereferencedbymanifestlist, imagetagdoesnotmatchdigest, invalidimagedigest, invalidimagetag, kmserror, missingdigestandtag"

instance ToText ImageFailureCode where
  toText = \case
    ImageNotFound -> "ImageNotFound"
    ImageReferencedByManifestList -> "ImageReferencedByManifestList"
    ImageTagDoesNotMatchDigest -> "ImageTagDoesNotMatchDigest"
    InvalidImageDigest -> "InvalidImageDigest"
    InvalidImageTag -> "InvalidImageTag"
    KMSError -> "KmsError"
    MissingDigestAndTag -> "MissingDigestAndTag"

instance Hashable ImageFailureCode

instance NFData ImageFailureCode

instance ToByteString ImageFailureCode

instance ToQuery ImageFailureCode

instance ToHeader ImageFailureCode

instance FromJSON ImageFailureCode where
  parseJSON = parseJSONText "ImageFailureCode"
