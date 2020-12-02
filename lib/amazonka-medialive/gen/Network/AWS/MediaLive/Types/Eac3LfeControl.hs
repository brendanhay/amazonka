{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3LfeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3LfeControl where

import Network.AWS.Prelude

-- | Eac3 Lfe Control
data Eac3LfeControl
  = Lfe
  | NoLfe
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

instance FromText Eac3LfeControl where
  parser =
    takeLowerText >>= \case
      "lfe" -> pure Lfe
      "no_lfe" -> pure NoLfe
      e ->
        fromTextError $
          "Failure parsing Eac3LfeControl from value: '" <> e
            <> "'. Accepted values: lfe, no_lfe"

instance ToText Eac3LfeControl where
  toText = \case
    Lfe -> "LFE"
    NoLfe -> "NO_LFE"

instance Hashable Eac3LfeControl

instance NFData Eac3LfeControl

instance ToByteString Eac3LfeControl

instance ToQuery Eac3LfeControl

instance ToHeader Eac3LfeControl

instance ToJSON Eac3LfeControl where
  toJSON = toJSONText

instance FromJSON Eac3LfeControl where
  parseJSON = parseJSONText "Eac3LfeControl"
