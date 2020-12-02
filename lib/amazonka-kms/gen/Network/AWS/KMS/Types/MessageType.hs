{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.MessageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.MessageType where

import Network.AWS.Prelude

data MessageType
  = Digest
  | Raw
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

instance FromText MessageType where
  parser =
    takeLowerText >>= \case
      "digest" -> pure Digest
      "raw" -> pure Raw
      e ->
        fromTextError $
          "Failure parsing MessageType from value: '" <> e
            <> "'. Accepted values: digest, raw"

instance ToText MessageType where
  toText = \case
    Digest -> "DIGEST"
    Raw -> "RAW"

instance Hashable MessageType

instance NFData MessageType

instance ToByteString MessageType

instance ToQuery MessageType

instance ToHeader MessageType

instance ToJSON MessageType where
  toJSON = toJSONText
