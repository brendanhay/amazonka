{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectCannedACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectCannedACL where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectCannedACL
  = OAWSExecRead
  | OAuthenticatedRead
  | OBucketOwnerFullControl
  | OBucketOwnerRead
  | OPrivate
  | OPublicRead
  | OPublicReadWrite
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

instance FromText ObjectCannedACL where
  parser =
    takeLowerText >>= \case
      "aws-exec-read" -> pure OAWSExecRead
      "authenticated-read" -> pure OAuthenticatedRead
      "bucket-owner-full-control" -> pure OBucketOwnerFullControl
      "bucket-owner-read" -> pure OBucketOwnerRead
      "private" -> pure OPrivate
      "public-read" -> pure OPublicRead
      "public-read-write" -> pure OPublicReadWrite
      e ->
        fromTextError $
          "Failure parsing ObjectCannedACL from value: '" <> e
            <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText ObjectCannedACL where
  toText = \case
    OAWSExecRead -> "aws-exec-read"
    OAuthenticatedRead -> "authenticated-read"
    OBucketOwnerFullControl -> "bucket-owner-full-control"
    OBucketOwnerRead -> "bucket-owner-read"
    OPrivate -> "private"
    OPublicRead -> "public-read"
    OPublicReadWrite -> "public-read-write"

instance Hashable ObjectCannedACL

instance NFData ObjectCannedACL

instance ToByteString ObjectCannedACL

instance ToQuery ObjectCannedACL

instance ToHeader ObjectCannedACL

instance ToXML ObjectCannedACL where
  toXML = toXMLText
