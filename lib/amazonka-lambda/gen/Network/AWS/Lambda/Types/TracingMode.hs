{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.TracingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingMode where

import Network.AWS.Prelude

data TracingMode
  = TMActive
  | TMPassThrough
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

instance FromText TracingMode where
  parser =
    takeLowerText >>= \case
      "active" -> pure TMActive
      "passthrough" -> pure TMPassThrough
      e ->
        fromTextError $
          "Failure parsing TracingMode from value: '" <> e
            <> "'. Accepted values: active, passthrough"

instance ToText TracingMode where
  toText = \case
    TMActive -> "Active"
    TMPassThrough -> "PassThrough"

instance Hashable TracingMode

instance NFData TracingMode

instance ToByteString TracingMode

instance ToQuery TracingMode

instance ToHeader TracingMode

instance ToJSON TracingMode where
  toJSON = toJSONText

instance FromJSON TracingMode where
  parseJSON = parseJSONText "TracingMode"
