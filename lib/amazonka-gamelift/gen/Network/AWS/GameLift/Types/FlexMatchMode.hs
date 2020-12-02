{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FlexMatchMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FlexMatchMode where

import Network.AWS.Prelude

data FlexMatchMode
  = Standalone
  | WithQueue
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

instance FromText FlexMatchMode where
  parser =
    takeLowerText >>= \case
      "standalone" -> pure Standalone
      "with_queue" -> pure WithQueue
      e ->
        fromTextError $
          "Failure parsing FlexMatchMode from value: '" <> e
            <> "'. Accepted values: standalone, with_queue"

instance ToText FlexMatchMode where
  toText = \case
    Standalone -> "STANDALONE"
    WithQueue -> "WITH_QUEUE"

instance Hashable FlexMatchMode

instance NFData FlexMatchMode

instance ToByteString FlexMatchMode

instance ToQuery FlexMatchMode

instance ToHeader FlexMatchMode

instance ToJSON FlexMatchMode where
  toJSON = toJSONText

instance FromJSON FlexMatchMode where
  parseJSON = parseJSONText "FlexMatchMode"
