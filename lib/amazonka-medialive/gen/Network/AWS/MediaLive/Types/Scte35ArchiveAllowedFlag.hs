{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag where

import Network.AWS.Prelude

-- | Corresponds to the archive_allowed parameter. A value of ARCHIVE_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35ArchiveAllowedFlag
  = ArchiveAllowed
  | ArchiveNotAllowed
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

instance FromText Scte35ArchiveAllowedFlag where
  parser =
    takeLowerText >>= \case
      "archive_allowed" -> pure ArchiveAllowed
      "archive_not_allowed" -> pure ArchiveNotAllowed
      e ->
        fromTextError $
          "Failure parsing Scte35ArchiveAllowedFlag from value: '" <> e
            <> "'. Accepted values: archive_allowed, archive_not_allowed"

instance ToText Scte35ArchiveAllowedFlag where
  toText = \case
    ArchiveAllowed -> "ARCHIVE_ALLOWED"
    ArchiveNotAllowed -> "ARCHIVE_NOT_ALLOWED"

instance Hashable Scte35ArchiveAllowedFlag

instance NFData Scte35ArchiveAllowedFlag

instance ToByteString Scte35ArchiveAllowedFlag

instance ToQuery Scte35ArchiveAllowedFlag

instance ToHeader Scte35ArchiveAllowedFlag

instance ToJSON Scte35ArchiveAllowedFlag where
  toJSON = toJSONText

instance FromJSON Scte35ArchiveAllowedFlag where
  parseJSON = parseJSONText "Scte35ArchiveAllowedFlag"
