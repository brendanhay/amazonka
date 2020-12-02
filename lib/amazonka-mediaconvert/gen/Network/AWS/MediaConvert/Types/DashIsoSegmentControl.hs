{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoSegmentControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoSegmentControl where

import Network.AWS.Prelude

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
data DashIsoSegmentControl
  = DISCSegmentedFiles
  | DISCSingleFile
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

instance FromText DashIsoSegmentControl where
  parser =
    takeLowerText >>= \case
      "segmented_files" -> pure DISCSegmentedFiles
      "single_file" -> pure DISCSingleFile
      e ->
        fromTextError $
          "Failure parsing DashIsoSegmentControl from value: '" <> e
            <> "'. Accepted values: segmented_files, single_file"

instance ToText DashIsoSegmentControl where
  toText = \case
    DISCSegmentedFiles -> "SEGMENTED_FILES"
    DISCSingleFile -> "SINGLE_FILE"

instance Hashable DashIsoSegmentControl

instance NFData DashIsoSegmentControl

instance ToByteString DashIsoSegmentControl

instance ToQuery DashIsoSegmentControl

instance ToHeader DashIsoSegmentControl

instance ToJSON DashIsoSegmentControl where
  toJSON = toJSONText

instance FromJSON DashIsoSegmentControl where
  parseJSON = parseJSONText "DashIsoSegmentControl"
