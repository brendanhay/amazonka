{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ObjectCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3ObjectCannedACL where

import Network.AWS.Prelude

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
data S3ObjectCannedACL
  = AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
  | PublicRead
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

instance FromText S3ObjectCannedACL where
  parser =
    takeLowerText >>= \case
      "authenticated_read" -> pure AuthenticatedRead
      "bucket_owner_full_control" -> pure BucketOwnerFullControl
      "bucket_owner_read" -> pure BucketOwnerRead
      "public_read" -> pure PublicRead
      e ->
        fromTextError $
          "Failure parsing S3ObjectCannedACL from value: '" <> e
            <> "'. Accepted values: authenticated_read, bucket_owner_full_control, bucket_owner_read, public_read"

instance ToText S3ObjectCannedACL where
  toText = \case
    AuthenticatedRead -> "AUTHENTICATED_READ"
    BucketOwnerFullControl -> "BUCKET_OWNER_FULL_CONTROL"
    BucketOwnerRead -> "BUCKET_OWNER_READ"
    PublicRead -> "PUBLIC_READ"

instance Hashable S3ObjectCannedACL

instance NFData S3ObjectCannedACL

instance ToByteString S3ObjectCannedACL

instance ToQuery S3ObjectCannedACL

instance ToHeader S3ObjectCannedACL

instance ToJSON S3ObjectCannedACL where
  toJSON = toJSONText

instance FromJSON S3ObjectCannedACL where
  parseJSON = parseJSONText "S3ObjectCannedACL"
