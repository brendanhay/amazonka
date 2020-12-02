{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.AssignPublicIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.AssignPublicIP where

import Network.AWS.Prelude

data AssignPublicIP
  = APIDisabled
  | APIEnabled
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

instance FromText AssignPublicIP where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure APIDisabled
      "enabled" -> pure APIEnabled
      e ->
        fromTextError $
          "Failure parsing AssignPublicIP from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText AssignPublicIP where
  toText = \case
    APIDisabled -> "DISABLED"
    APIEnabled -> "ENABLED"

instance Hashable AssignPublicIP

instance NFData AssignPublicIP

instance ToByteString AssignPublicIP

instance ToQuery AssignPublicIP

instance ToHeader AssignPublicIP

instance ToJSON AssignPublicIP where
  toJSON = toJSONText

instance FromJSON AssignPublicIP where
  parseJSON = parseJSONText "AssignPublicIP"
