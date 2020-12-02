{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265AlternativeTransferFunction where

import Network.AWS.Prelude

-- | H265 Alternative Transfer Function
data H265AlternativeTransferFunction
  = Insert
  | Omit
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

instance FromText H265AlternativeTransferFunction where
  parser =
    takeLowerText >>= \case
      "insert" -> pure Insert
      "omit" -> pure Omit
      e ->
        fromTextError $
          "Failure parsing H265AlternativeTransferFunction from value: '" <> e
            <> "'. Accepted values: insert, omit"

instance ToText H265AlternativeTransferFunction where
  toText = \case
    Insert -> "INSERT"
    Omit -> "OMIT"

instance Hashable H265AlternativeTransferFunction

instance NFData H265AlternativeTransferFunction

instance ToByteString H265AlternativeTransferFunction

instance ToQuery H265AlternativeTransferFunction

instance ToHeader H265AlternativeTransferFunction

instance ToJSON H265AlternativeTransferFunction where
  toJSON = toJSONText

instance FromJSON H265AlternativeTransferFunction where
  parseJSON = parseJSONText "H265AlternativeTransferFunction"
