{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.CloudHSMObjectState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.CloudHSMObjectState where

import Network.AWS.Prelude

data CloudHSMObjectState
  = Degraded
  | Ready
  | Updating
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

instance FromText CloudHSMObjectState where
  parser =
    takeLowerText >>= \case
      "degraded" -> pure Degraded
      "ready" -> pure Ready
      "updating" -> pure Updating
      e ->
        fromTextError $
          "Failure parsing CloudHSMObjectState from value: '" <> e
            <> "'. Accepted values: degraded, ready, updating"

instance ToText CloudHSMObjectState where
  toText = \case
    Degraded -> "DEGRADED"
    Ready -> "READY"
    Updating -> "UPDATING"

instance Hashable CloudHSMObjectState

instance NFData CloudHSMObjectState

instance ToByteString CloudHSMObjectState

instance ToQuery CloudHSMObjectState

instance ToHeader CloudHSMObjectState

instance FromJSON CloudHSMObjectState where
  parseJSON = parseJSONText "CloudHSMObjectState"
