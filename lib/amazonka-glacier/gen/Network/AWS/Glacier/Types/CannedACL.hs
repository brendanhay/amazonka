{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.CannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.CannedACL where

import Network.AWS.Prelude

data CannedACL
  = AWSExecRead
  | AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
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

instance FromText CannedACL where
  parser =
    takeLowerText >>= \case
      "aws-exec-read" -> pure AWSExecRead
      "authenticated-read" -> pure AuthenticatedRead
      "bucket-owner-full-control" -> pure BucketOwnerFullControl
      "bucket-owner-read" -> pure BucketOwnerRead
      "private" -> pure Private
      "public-read" -> pure PublicRead
      "public-read-write" -> pure PublicReadWrite
      e ->
        fromTextError $
          "Failure parsing CannedACL from value: '" <> e
            <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText CannedACL where
  toText = \case
    AWSExecRead -> "aws-exec-read"
    AuthenticatedRead -> "authenticated-read"
    BucketOwnerFullControl -> "bucket-owner-full-control"
    BucketOwnerRead -> "bucket-owner-read"
    Private -> "private"
    PublicRead -> "public-read"
    PublicReadWrite -> "public-read-write"

instance Hashable CannedACL

instance NFData CannedACL

instance ToByteString CannedACL

instance ToQuery CannedACL

instance ToHeader CannedACL

instance ToJSON CannedACL where
  toJSON = toJSONText

instance FromJSON CannedACL where
  parseJSON = parseJSONText "CannedACL"
