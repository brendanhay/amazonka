{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountState where

import Network.AWS.Prelude

data CreateAccountState
  = Failed
  | InProgress
  | Succeeded
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

instance FromText CreateAccountState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing CreateAccountState from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText CreateAccountState where
  toText = \case
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    Succeeded -> "SUCCEEDED"

instance Hashable CreateAccountState

instance NFData CreateAccountState

instance ToByteString CreateAccountState

instance ToQuery CreateAccountState

instance ToHeader CreateAccountState

instance ToJSON CreateAccountState where
  toJSON = toJSONText

instance FromJSON CreateAccountState where
  parseJSON = parseJSONText "CreateAccountState"
