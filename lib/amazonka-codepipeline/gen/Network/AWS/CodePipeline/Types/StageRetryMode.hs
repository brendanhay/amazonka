{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageRetryMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageRetryMode where

import Network.AWS.Prelude

data StageRetryMode = FailedActions
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

instance FromText StageRetryMode where
  parser =
    takeLowerText >>= \case
      "failed_actions" -> pure FailedActions
      e ->
        fromTextError $
          "Failure parsing StageRetryMode from value: '" <> e
            <> "'. Accepted values: failed_actions"

instance ToText StageRetryMode where
  toText = \case
    FailedActions -> "FAILED_ACTIONS"

instance Hashable StageRetryMode

instance NFData StageRetryMode

instance ToByteString StageRetryMode

instance ToQuery StageRetryMode

instance ToHeader StageRetryMode

instance ToJSON StageRetryMode where
  toJSON = toJSONText
