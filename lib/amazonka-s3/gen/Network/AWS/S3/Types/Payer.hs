{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Payer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Payer where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Payer
  = BucketOwner
  | Requester
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

instance FromText Payer where
  parser =
    takeLowerText >>= \case
      "bucketowner" -> pure BucketOwner
      "requester" -> pure Requester
      e ->
        fromTextError $
          "Failure parsing Payer from value: '" <> e
            <> "'. Accepted values: bucketowner, requester"

instance ToText Payer where
  toText = \case
    BucketOwner -> "BucketOwner"
    Requester -> "Requester"

instance Hashable Payer

instance NFData Payer

instance ToByteString Payer

instance ToQuery Payer

instance ToHeader Payer

instance FromXML Payer where
  parseXML = parseXMLText "Payer"

instance ToXML Payer where
  toXML = toXMLText
