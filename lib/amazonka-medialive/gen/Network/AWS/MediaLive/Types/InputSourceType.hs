{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceType where

import Network.AWS.Prelude

-- | There are two types of input sources, static and dynamic. If an input source is dynamic you can
--
-- change the source url of the input dynamically using an input switch action. However, the only input type
-- to support a dynamic url at this time is MP4_FILE. By default all input sources are static.
data InputSourceType
  = ISTDynamic
  | ISTStatic
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

instance FromText InputSourceType where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure ISTDynamic
      "static" -> pure ISTStatic
      e ->
        fromTextError $
          "Failure parsing InputSourceType from value: '" <> e
            <> "'. Accepted values: dynamic, static"

instance ToText InputSourceType where
  toText = \case
    ISTDynamic -> "DYNAMIC"
    ISTStatic -> "STATIC"

instance Hashable InputSourceType

instance NFData InputSourceType

instance ToByteString InputSourceType

instance ToQuery InputSourceType

instance ToHeader InputSourceType

instance FromJSON InputSourceType where
  parseJSON = parseJSONText "InputSourceType"
