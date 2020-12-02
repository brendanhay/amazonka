{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafSegmentControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafSegmentControl where

import Network.AWS.Prelude

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
data CmafSegmentControl
  = SegmentedFiles
  | SingleFile
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

instance FromText CmafSegmentControl where
  parser =
    takeLowerText >>= \case
      "segmented_files" -> pure SegmentedFiles
      "single_file" -> pure SingleFile
      e ->
        fromTextError $
          "Failure parsing CmafSegmentControl from value: '" <> e
            <> "'. Accepted values: segmented_files, single_file"

instance ToText CmafSegmentControl where
  toText = \case
    SegmentedFiles -> "SEGMENTED_FILES"
    SingleFile -> "SINGLE_FILE"

instance Hashable CmafSegmentControl

instance NFData CmafSegmentControl

instance ToByteString CmafSegmentControl

instance ToQuery CmafSegmentControl

instance ToHeader CmafSegmentControl

instance ToJSON CmafSegmentControl where
  toJSON = toJSONText

instance FromJSON CmafSegmentControl where
  parseJSON = parseJSONText "CmafSegmentControl"
