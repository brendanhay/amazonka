{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Sum where

import Network.AWS.Prelude

-- | Sets the access control list permission for objects in the S3 bucket that a file gateway puts objects into. The default value is "private".
--
--
data ObjectACL
  = AWSExecRead
  | AuthenticatedRead
  | BucketOwnerFullControl
  | BucketOwnerRead
  | Private
  | PublicRead
  | PublicReadWrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ObjectACL where
    parser = takeLowerText >>= \case
        "aws-exec-read" -> pure AWSExecRead
        "authenticated-read" -> pure AuthenticatedRead
        "bucket-owner-full-control" -> pure BucketOwnerFullControl
        "bucket-owner-read" -> pure BucketOwnerRead
        "private" -> pure Private
        "public-read" -> pure PublicRead
        "public-read-write" -> pure PublicReadWrite
        e -> fromTextError $ "Failure parsing ObjectACL from value: '" <> e
           <> "'. Accepted values: aws-exec-read, authenticated-read, bucket-owner-full-control, bucket-owner-read, private, public-read, public-read-write"

instance ToText ObjectACL where
    toText = \case
        AWSExecRead -> "aws-exec-read"
        AuthenticatedRead -> "authenticated-read"
        BucketOwnerFullControl -> "bucket-owner-full-control"
        BucketOwnerRead -> "bucket-owner-read"
        Private -> "private"
        PublicRead -> "public-read"
        PublicReadWrite -> "public-read-write"

instance Hashable     ObjectACL
instance NFData       ObjectACL
instance ToByteString ObjectACL
instance ToQuery      ObjectACL
instance ToHeader     ObjectACL

instance ToJSON ObjectACL where
    toJSON = toJSONText

instance FromJSON ObjectACL where
    parseJSON = parseJSONText "ObjectACL"
