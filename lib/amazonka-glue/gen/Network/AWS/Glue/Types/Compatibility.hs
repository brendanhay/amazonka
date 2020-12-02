{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Compatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Compatibility where

import Network.AWS.Prelude

data Compatibility
  = CBackward
  | CBackwardAll
  | CDisabled
  | CForward
  | CForwardAll
  | CFull
  | CFullAll
  | CNone
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

instance FromText Compatibility where
  parser =
    takeLowerText >>= \case
      "backward" -> pure CBackward
      "backward_all" -> pure CBackwardAll
      "disabled" -> pure CDisabled
      "forward" -> pure CForward
      "forward_all" -> pure CForwardAll
      "full" -> pure CFull
      "full_all" -> pure CFullAll
      "none" -> pure CNone
      e ->
        fromTextError $
          "Failure parsing Compatibility from value: '" <> e
            <> "'. Accepted values: backward, backward_all, disabled, forward, forward_all, full, full_all, none"

instance ToText Compatibility where
  toText = \case
    CBackward -> "BACKWARD"
    CBackwardAll -> "BACKWARD_ALL"
    CDisabled -> "DISABLED"
    CForward -> "FORWARD"
    CForwardAll -> "FORWARD_ALL"
    CFull -> "FULL"
    CFullAll -> "FULL_ALL"
    CNone -> "NONE"

instance Hashable Compatibility

instance NFData Compatibility

instance ToByteString Compatibility

instance ToQuery Compatibility

instance ToHeader Compatibility

instance ToJSON Compatibility where
  toJSON = toJSONText

instance FromJSON Compatibility where
  parseJSON = parseJSONText "Compatibility"
