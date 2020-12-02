{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectOwnership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectOwnership where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The container element for object ownership for a bucket's ownership controls.
--
--
-- BucketOwnerPreferred - Objects uploaded to the bucket change ownership to the bucket owner if the objects are uploaded with the @bucket-owner-full-control@ canned ACL.
--
-- ObjectWriter - The uploading account will own the object if the object is uploaded with the @bucket-owner-full-control@ canned ACL.
data ObjectOwnership
  = BucketOwnerPreferred
  | ObjectWriter
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

instance FromText ObjectOwnership where
  parser =
    takeLowerText >>= \case
      "bucketownerpreferred" -> pure BucketOwnerPreferred
      "objectwriter" -> pure ObjectWriter
      e ->
        fromTextError $
          "Failure parsing ObjectOwnership from value: '" <> e
            <> "'. Accepted values: bucketownerpreferred, objectwriter"

instance ToText ObjectOwnership where
  toText = \case
    BucketOwnerPreferred -> "BucketOwnerPreferred"
    ObjectWriter -> "ObjectWriter"

instance Hashable ObjectOwnership

instance NFData ObjectOwnership

instance ToByteString ObjectOwnership

instance ToQuery ObjectOwnership

instance ToHeader ObjectOwnership

instance FromXML ObjectOwnership where
  parseXML = parseXMLText "ObjectOwnership"

instance ToXML ObjectOwnership where
  toXML = toXMLText
