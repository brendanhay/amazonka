{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ArchiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ArchiveStatus where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ArchiveStatus
  = ASArchiveAccess
  | ASDeepArchiveAccess
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

instance FromText ArchiveStatus where
  parser =
    takeLowerText >>= \case
      "archive_access" -> pure ASArchiveAccess
      "deep_archive_access" -> pure ASDeepArchiveAccess
      e ->
        fromTextError $
          "Failure parsing ArchiveStatus from value: '" <> e
            <> "'. Accepted values: archive_access, deep_archive_access"

instance ToText ArchiveStatus where
  toText = \case
    ASArchiveAccess -> "ARCHIVE_ACCESS"
    ASDeepArchiveAccess -> "DEEP_ARCHIVE_ACCESS"

instance Hashable ArchiveStatus

instance NFData ArchiveStatus

instance ToByteString ArchiveStatus

instance ToQuery ArchiveStatus

instance ToHeader ArchiveStatus

instance FromXML ArchiveStatus where
  parseXML = parseXMLText "ArchiveStatus"
