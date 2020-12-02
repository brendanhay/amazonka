{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsKlv
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsKlv where

import Network.AWS.Prelude

-- | M2ts Klv
data M2tsKlv
  = MKNone
  | MKPassthrough
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

instance FromText M2tsKlv where
  parser =
    takeLowerText >>= \case
      "none" -> pure MKNone
      "passthrough" -> pure MKPassthrough
      e ->
        fromTextError $
          "Failure parsing M2tsKlv from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText M2tsKlv where
  toText = \case
    MKNone -> "NONE"
    MKPassthrough -> "PASSTHROUGH"

instance Hashable M2tsKlv

instance NFData M2tsKlv

instance ToByteString M2tsKlv

instance ToQuery M2tsKlv

instance ToHeader M2tsKlv

instance ToJSON M2tsKlv where
  toJSON = toJSONText

instance FromJSON M2tsKlv where
  parseJSON = parseJSONText "M2tsKlv"
