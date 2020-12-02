{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp where

import Network.AWS.Prelude

data HLSDisplayFragmentTimestamp
  = HLSDFTAlways
  | HLSDFTNever
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

instance FromText HLSDisplayFragmentTimestamp where
  parser =
    takeLowerText >>= \case
      "always" -> pure HLSDFTAlways
      "never" -> pure HLSDFTNever
      e ->
        fromTextError $
          "Failure parsing HLSDisplayFragmentTimestamp from value: '" <> e
            <> "'. Accepted values: always, never"

instance ToText HLSDisplayFragmentTimestamp where
  toText = \case
    HLSDFTAlways -> "ALWAYS"
    HLSDFTNever -> "NEVER"

instance Hashable HLSDisplayFragmentTimestamp

instance NFData HLSDisplayFragmentTimestamp

instance ToByteString HLSDisplayFragmentTimestamp

instance ToQuery HLSDisplayFragmentTimestamp

instance ToHeader HLSDisplayFragmentTimestamp

instance ToJSON HLSDisplayFragmentTimestamp where
  toJSON = toJSONText
