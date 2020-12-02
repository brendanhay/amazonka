{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketCannedACL where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketCannedACL
  = BAuthenticatedRead
  | BPrivate
  | BPublicRead
  | BPublicReadWrite
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

instance FromText BucketCannedACL where
  parser =
    takeLowerText >>= \case
      "authenticated-read" -> pure BAuthenticatedRead
      "private" -> pure BPrivate
      "public-read" -> pure BPublicRead
      "public-read-write" -> pure BPublicReadWrite
      e ->
        fromTextError $
          "Failure parsing BucketCannedACL from value: '" <> e
            <> "'. Accepted values: authenticated-read, private, public-read, public-read-write"

instance ToText BucketCannedACL where
  toText = \case
    BAuthenticatedRead -> "authenticated-read"
    BPrivate -> "private"
    BPublicRead -> "public-read"
    BPublicReadWrite -> "public-read-write"

instance Hashable BucketCannedACL

instance NFData BucketCannedACL

instance ToByteString BucketCannedACL

instance ToQuery BucketCannedACL

instance ToHeader BucketCannedACL

instance ToXML BucketCannedACL where
  toXML = toXMLText
