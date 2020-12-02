{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringAccessTier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringAccessTier where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data IntelligentTieringAccessTier
  = ArchiveAccess
  | DeepArchiveAccess
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

instance FromText IntelligentTieringAccessTier where
  parser =
    takeLowerText >>= \case
      "archive_access" -> pure ArchiveAccess
      "deep_archive_access" -> pure DeepArchiveAccess
      e ->
        fromTextError $
          "Failure parsing IntelligentTieringAccessTier from value: '" <> e
            <> "'. Accepted values: archive_access, deep_archive_access"

instance ToText IntelligentTieringAccessTier where
  toText = \case
    ArchiveAccess -> "ARCHIVE_ACCESS"
    DeepArchiveAccess -> "DEEP_ARCHIVE_ACCESS"

instance Hashable IntelligentTieringAccessTier

instance NFData IntelligentTieringAccessTier

instance ToByteString IntelligentTieringAccessTier

instance ToQuery IntelligentTieringAccessTier

instance ToHeader IntelligentTieringAccessTier

instance FromXML IntelligentTieringAccessTier where
  parseXML = parseXMLText "IntelligentTieringAccessTier"

instance ToXML IntelligentTieringAccessTier where
  toXML = toXMLText
