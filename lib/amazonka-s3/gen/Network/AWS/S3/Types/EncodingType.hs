{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EncodingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.EncodingType where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Requests Amazon S3 to encode the object keys in the response and specifies the encoding method to use. An object key may contain any Unicode character; however, XML 1.0 parser cannot parse some characters, such as characters with an ASCII value from 0 to 10. For characters that are not supported in XML 1.0, you can add this parameter to request that Amazon S3 encode the keys in the response.
data EncodingType = URL
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

instance FromText EncodingType where
  parser =
    takeLowerText >>= \case
      "url" -> pure URL
      e ->
        fromTextError $
          "Failure parsing EncodingType from value: '" <> e
            <> "'. Accepted values: url"

instance ToText EncodingType where
  toText = \case
    URL -> "url"

instance Hashable EncodingType

instance NFData EncodingType

instance ToByteString EncodingType

instance ToQuery EncodingType

instance ToHeader EncodingType

instance FromXML EncodingType where
  parseXML = parseXMLText "EncodingType"

instance ToXML EncodingType where
  toXML = toXMLText
