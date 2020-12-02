{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputTimecodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputTimecodeSource where

import Network.AWS.Prelude

-- | Documentation update needed
data InputTimecodeSource
  = Embedded
  | Zerobased
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

instance FromText InputTimecodeSource where
  parser =
    takeLowerText >>= \case
      "embedded" -> pure Embedded
      "zerobased" -> pure Zerobased
      e ->
        fromTextError $
          "Failure parsing InputTimecodeSource from value: '" <> e
            <> "'. Accepted values: embedded, zerobased"

instance ToText InputTimecodeSource where
  toText = \case
    Embedded -> "EMBEDDED"
    Zerobased -> "ZEROBASED"

instance Hashable InputTimecodeSource

instance NFData InputTimecodeSource

instance ToByteString InputTimecodeSource

instance ToQuery InputTimecodeSource

instance ToHeader InputTimecodeSource

instance ToJSON InputTimecodeSource where
  toJSON = toJSONText

instance FromJSON InputTimecodeSource where
  parseJSON = parseJSONText "InputTimecodeSource"
