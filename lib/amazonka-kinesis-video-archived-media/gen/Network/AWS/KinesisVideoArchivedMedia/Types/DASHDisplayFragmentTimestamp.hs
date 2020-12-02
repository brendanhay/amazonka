{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentTimestamp where

import Network.AWS.Prelude

data DASHDisplayFragmentTimestamp
  = DASHDFTAlways
  | DASHDFTNever
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

instance FromText DASHDisplayFragmentTimestamp where
  parser =
    takeLowerText >>= \case
      "always" -> pure DASHDFTAlways
      "never" -> pure DASHDFTNever
      e ->
        fromTextError $
          "Failure parsing DASHDisplayFragmentTimestamp from value: '" <> e
            <> "'. Accepted values: always, never"

instance ToText DASHDisplayFragmentTimestamp where
  toText = \case
    DASHDFTAlways -> "ALWAYS"
    DASHDFTNever -> "NEVER"

instance Hashable DASHDisplayFragmentTimestamp

instance NFData DASHDisplayFragmentTimestamp

instance ToByteString DASHDisplayFragmentTimestamp

instance ToQuery DASHDisplayFragmentTimestamp

instance ToHeader DASHDisplayFragmentTimestamp

instance ToJSON DASHDisplayFragmentTimestamp where
  toJSON = toJSONText
