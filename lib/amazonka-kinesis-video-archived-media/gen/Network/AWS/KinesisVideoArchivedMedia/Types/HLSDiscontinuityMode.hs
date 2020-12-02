{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSDiscontinuityMode where

import Network.AWS.Prelude

data HLSDiscontinuityMode
  = Always
  | Never
  | OnDiscontinuity
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

instance FromText HLSDiscontinuityMode where
  parser =
    takeLowerText >>= \case
      "always" -> pure Always
      "never" -> pure Never
      "on_discontinuity" -> pure OnDiscontinuity
      e ->
        fromTextError $
          "Failure parsing HLSDiscontinuityMode from value: '" <> e
            <> "'. Accepted values: always, never, on_discontinuity"

instance ToText HLSDiscontinuityMode where
  toText = \case
    Always -> "ALWAYS"
    Never -> "NEVER"
    OnDiscontinuity -> "ON_DISCONTINUITY"

instance Hashable HLSDiscontinuityMode

instance NFData HLSDiscontinuityMode

instance ToByteString HLSDiscontinuityMode

instance ToQuery HLSDiscontinuityMode

instance ToHeader HLSDiscontinuityMode

instance ToJSON HLSDiscontinuityMode where
  toJSON = toJSONText
