{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AcceptanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AcceptanceType where

import Network.AWS.Prelude

data AcceptanceType
  = Accept
  | Reject
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

instance FromText AcceptanceType where
  parser =
    takeLowerText >>= \case
      "accept" -> pure Accept
      "reject" -> pure Reject
      e ->
        fromTextError $
          "Failure parsing AcceptanceType from value: '" <> e
            <> "'. Accepted values: accept, reject"

instance ToText AcceptanceType where
  toText = \case
    Accept -> "ACCEPT"
    Reject -> "REJECT"

instance Hashable AcceptanceType

instance NFData AcceptanceType

instance ToByteString AcceptanceType

instance ToQuery AcceptanceType

instance ToHeader AcceptanceType

instance ToJSON AcceptanceType where
  toJSON = toJSONText
