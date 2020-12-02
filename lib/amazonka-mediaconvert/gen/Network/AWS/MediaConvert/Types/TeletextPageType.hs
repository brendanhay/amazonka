{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextPageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextPageType where

import Network.AWS.Prelude

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
data TeletextPageType
  = PageTypeAddlInfo
  | PageTypeHearingImpairedSubtitle
  | PageTypeInitial
  | PageTypeProgramSchedule
  | PageTypeSubtitle
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

instance FromText TeletextPageType where
  parser =
    takeLowerText >>= \case
      "page_type_addl_info" -> pure PageTypeAddlInfo
      "page_type_hearing_impaired_subtitle" -> pure PageTypeHearingImpairedSubtitle
      "page_type_initial" -> pure PageTypeInitial
      "page_type_program_schedule" -> pure PageTypeProgramSchedule
      "page_type_subtitle" -> pure PageTypeSubtitle
      e ->
        fromTextError $
          "Failure parsing TeletextPageType from value: '" <> e
            <> "'. Accepted values: page_type_addl_info, page_type_hearing_impaired_subtitle, page_type_initial, page_type_program_schedule, page_type_subtitle"

instance ToText TeletextPageType where
  toText = \case
    PageTypeAddlInfo -> "PAGE_TYPE_ADDL_INFO"
    PageTypeHearingImpairedSubtitle -> "PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE"
    PageTypeInitial -> "PAGE_TYPE_INITIAL"
    PageTypeProgramSchedule -> "PAGE_TYPE_PROGRAM_SCHEDULE"
    PageTypeSubtitle -> "PAGE_TYPE_SUBTITLE"

instance Hashable TeletextPageType

instance NFData TeletextPageType

instance ToByteString TeletextPageType

instance ToQuery TeletextPageType

instance ToHeader TeletextPageType

instance ToJSON TeletextPageType where
  toJSON = toJSONText

instance FromJSON TeletextPageType where
  parseJSON = parseJSONText "TeletextPageType"
