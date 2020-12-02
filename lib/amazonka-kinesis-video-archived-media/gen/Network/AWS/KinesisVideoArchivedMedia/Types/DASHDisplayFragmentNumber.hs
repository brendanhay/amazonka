{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHDisplayFragmentNumber where

import Network.AWS.Prelude

data DASHDisplayFragmentNumber
  = DASHDFNAlways
  | DASHDFNNever
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

instance FromText DASHDisplayFragmentNumber where
  parser =
    takeLowerText >>= \case
      "always" -> pure DASHDFNAlways
      "never" -> pure DASHDFNNever
      e ->
        fromTextError $
          "Failure parsing DASHDisplayFragmentNumber from value: '" <> e
            <> "'. Accepted values: always, never"

instance ToText DASHDisplayFragmentNumber where
  toText = \case
    DASHDFNAlways -> "ALWAYS"
    DASHDFNNever -> "NEVER"

instance Hashable DASHDisplayFragmentNumber

instance NFData DASHDisplayFragmentNumber

instance ToByteString DASHDisplayFragmentNumber

instance ToQuery DASHDisplayFragmentNumber

instance ToHeader DASHDisplayFragmentNumber

instance ToJSON DASHDisplayFragmentNumber where
  toJSON = toJSONText
