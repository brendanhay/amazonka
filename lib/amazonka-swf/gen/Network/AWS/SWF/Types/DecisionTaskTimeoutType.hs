{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskTimeoutType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskTimeoutType where

import Network.AWS.Prelude

data DecisionTaskTimeoutType = StartToClose
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

instance FromText DecisionTaskTimeoutType where
  parser =
    takeLowerText >>= \case
      "start_to_close" -> pure StartToClose
      e ->
        fromTextError $
          "Failure parsing DecisionTaskTimeoutType from value: '" <> e
            <> "'. Accepted values: start_to_close"

instance ToText DecisionTaskTimeoutType where
  toText = \case
    StartToClose -> "START_TO_CLOSE"

instance Hashable DecisionTaskTimeoutType

instance NFData DecisionTaskTimeoutType

instance ToByteString DecisionTaskTimeoutType

instance ToQuery DecisionTaskTimeoutType

instance ToHeader DecisionTaskTimeoutType

instance FromJSON DecisionTaskTimeoutType where
  parseJSON = parseJSONText "DecisionTaskTimeoutType"
