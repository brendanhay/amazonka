{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.StopAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.StopAction where

import Network.AWS.Prelude

data StopAction
  = SkipEvaluation
  | StartEvaluation
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

instance FromText StopAction where
  parser =
    takeLowerText >>= \case
      "skip_evaluation" -> pure SkipEvaluation
      "start_evaluation" -> pure StartEvaluation
      e ->
        fromTextError $
          "Failure parsing StopAction from value: '" <> e
            <> "'. Accepted values: skip_evaluation, start_evaluation"

instance ToText StopAction where
  toText = \case
    SkipEvaluation -> "SKIP_EVALUATION"
    StartEvaluation -> "START_EVALUATION"

instance Hashable StopAction

instance NFData StopAction

instance ToByteString StopAction

instance ToQuery StopAction

instance ToHeader StopAction

instance ToJSON StopAction where
  toJSON = toJSONText
