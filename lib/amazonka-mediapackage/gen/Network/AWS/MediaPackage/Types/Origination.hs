{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Origination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Origination where

import Network.AWS.Prelude

data Origination
  = Allow
  | Deny
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

instance FromText Origination where
  parser =
    takeLowerText >>= \case
      "allow" -> pure Allow
      "deny" -> pure Deny
      e ->
        fromTextError $
          "Failure parsing Origination from value: '" <> e
            <> "'. Accepted values: allow, deny"

instance ToText Origination where
  toText = \case
    Allow -> "ALLOW"
    Deny -> "DENY"

instance Hashable Origination

instance NFData Origination

instance ToByteString Origination

instance ToQuery Origination

instance ToHeader Origination

instance ToJSON Origination where
  toJSON = toJSONText

instance FromJSON Origination where
  parseJSON = parseJSONText "Origination"
