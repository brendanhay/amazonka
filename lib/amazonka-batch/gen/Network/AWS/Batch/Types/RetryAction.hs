{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.RetryAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.RetryAction where

import Network.AWS.Prelude

data RetryAction
  = Exit
  | Retry
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

instance FromText RetryAction where
  parser =
    takeLowerText >>= \case
      "exit" -> pure Exit
      "retry" -> pure Retry
      e ->
        fromTextError $
          "Failure parsing RetryAction from value: '" <> e
            <> "'. Accepted values: exit, retry"

instance ToText RetryAction where
  toText = \case
    Exit -> "EXIT"
    Retry -> "RETRY"

instance Hashable RetryAction

instance NFData RetryAction

instance ToByteString RetryAction

instance ToQuery RetryAction

instance ToHeader RetryAction

instance ToJSON RetryAction where
  toJSON = toJSONText

instance FromJSON RetryAction where
  parseJSON = parseJSONText "RetryAction"
