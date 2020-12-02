{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Type where

import Network.AWS.Prelude

data Type
  = AmazonCustomerByEmail
  | CanonicalUser
  | Group
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

instance FromText Type where
  parser =
    takeLowerText >>= \case
      "amazoncustomerbyemail" -> pure AmazonCustomerByEmail
      "canonicaluser" -> pure CanonicalUser
      "group" -> pure Group
      e ->
        fromTextError $
          "Failure parsing Type from value: '" <> e
            <> "'. Accepted values: amazoncustomerbyemail, canonicaluser, group"

instance ToText Type where
  toText = \case
    AmazonCustomerByEmail -> "AmazonCustomerByEmail"
    CanonicalUser -> "CanonicalUser"
    Group -> "Group"

instance Hashable Type

instance NFData Type

instance ToByteString Type

instance ToQuery Type

instance ToHeader Type

instance ToJSON Type where
  toJSON = toJSONText

instance FromJSON Type where
  parseJSON = parseJSONText "Type"
