{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ConflictHandlerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ConflictHandlerType where

import Network.AWS.Prelude

data ConflictHandlerType
  = CHTAutomerge
  | CHTLambda
  | CHTNone
  | CHTOptimisticConcurrency
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

instance FromText ConflictHandlerType where
  parser =
    takeLowerText >>= \case
      "automerge" -> pure CHTAutomerge
      "lambda" -> pure CHTLambda
      "none" -> pure CHTNone
      "optimistic_concurrency" -> pure CHTOptimisticConcurrency
      e ->
        fromTextError $
          "Failure parsing ConflictHandlerType from value: '" <> e
            <> "'. Accepted values: automerge, lambda, none, optimistic_concurrency"

instance ToText ConflictHandlerType where
  toText = \case
    CHTAutomerge -> "AUTOMERGE"
    CHTLambda -> "LAMBDA"
    CHTNone -> "NONE"
    CHTOptimisticConcurrency -> "OPTIMISTIC_CONCURRENCY"

instance Hashable ConflictHandlerType

instance NFData ConflictHandlerType

instance ToByteString ConflictHandlerType

instance ToQuery ConflictHandlerType

instance ToHeader ConflictHandlerType

instance ToJSON ConflictHandlerType where
  toJSON = toJSONText

instance FromJSON ConflictHandlerType where
  parseJSON = parseJSONText "ConflictHandlerType"
