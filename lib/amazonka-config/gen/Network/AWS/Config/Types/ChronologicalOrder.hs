{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ChronologicalOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ChronologicalOrder where

import Network.AWS.Prelude

data ChronologicalOrder
  = Forward
  | Reverse
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

instance FromText ChronologicalOrder where
  parser =
    takeLowerText >>= \case
      "forward" -> pure Forward
      "reverse" -> pure Reverse
      e ->
        fromTextError $
          "Failure parsing ChronologicalOrder from value: '" <> e
            <> "'. Accepted values: forward, reverse"

instance ToText ChronologicalOrder where
  toText = \case
    Forward -> "Forward"
    Reverse -> "Reverse"

instance Hashable ChronologicalOrder

instance NFData ChronologicalOrder

instance ToByteString ChronologicalOrder

instance ToQuery ChronologicalOrder

instance ToHeader ChronologicalOrder

instance ToJSON ChronologicalOrder where
  toJSON = toJSONText
