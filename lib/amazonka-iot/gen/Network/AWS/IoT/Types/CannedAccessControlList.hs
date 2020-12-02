{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CannedAccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CannedAccessControlList where

import Network.AWS.Prelude

data CannedAccessControlList
  = AWSExecRead
  | AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
  | LogDeliveryWrite
  | Private
  | PublicRead
  | PublicReadWrite
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

instance FromText CannedAccessControlList where
  parser =
    takeLowerText >>= \case
      "aws-exec-read" -> pure AWSExecRead
      "authenticated-read" -> pure AuthenticatedRead
      "bucket-owner-full-control" -> pure BucketOwnerFullControl
      "bucket-owner-read" -> pure BucketOwnerRead
      "log-delivery-write" -> pure LogDeliveryWrite
      "private" -> pure Private
      "public-read" -> pure PublicRead
      "public-read-write" -> pure PublicReadWrite
      e ->
        fromTextError $
          "Failure parsing CannedAccessControlList from value: '" <> e
            <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, log-delivery-write, private, public-read, public-read-write"

instance ToText CannedAccessControlList where
  toText = \case
    AWSExecRead -> "aws-exec-read"
    AuthenticatedRead -> "authenticated-read"
    BucketOwnerFullControl -> "bucket-owner-full-control"
    BucketOwnerRead -> "bucket-owner-read"
    LogDeliveryWrite -> "log-delivery-write"
    Private -> "private"
    PublicRead -> "public-read"
    PublicReadWrite -> "public-read-write"

instance Hashable CannedAccessControlList

instance NFData CannedAccessControlList

instance ToByteString CannedAccessControlList

instance ToQuery CannedAccessControlList

instance ToHeader CannedAccessControlList

instance ToJSON CannedAccessControlList where
  toJSON = toJSONText

instance FromJSON CannedAccessControlList where
  parseJSON = parseJSONText "CannedAccessControlList"
