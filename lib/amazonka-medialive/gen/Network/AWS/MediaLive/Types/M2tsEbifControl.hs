{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsEbifControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsEbifControl where

import Network.AWS.Prelude

-- | M2ts Ebif Control
data M2tsEbifControl
  = MECNone
  | MECPassthrough
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

instance FromText M2tsEbifControl where
  parser =
    takeLowerText >>= \case
      "none" -> pure MECNone
      "passthrough" -> pure MECPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsEbifControl from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText M2tsEbifControl where
  toText = \case
    MECNone -> "NONE"
    MECPassthrough -> "PASSTHROUGH"

instance Hashable M2tsEbifControl

instance NFData M2tsEbifControl

instance ToByteString M2tsEbifControl

instance ToQuery M2tsEbifControl

instance ToHeader M2tsEbifControl

instance ToJSON M2tsEbifControl where
  toJSON = toJSONText

instance FromJSON M2tsEbifControl where
  parseJSON = parseJSONText "M2tsEbifControl"
