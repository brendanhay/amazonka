{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TeletextPageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextPageType
  ( TeletextPageType
      ( TeletextPageType',
        TeletextPageTypePageTypeInitial,
        TeletextPageTypePageTypeSubtitle,
        TeletextPageTypePageTypeAddlInfo,
        TeletextPageTypePageTypeProgramSchedule,
        TeletextPageTypePageTypeHearingImpairedSubtitle,
        fromTeletextPageType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
newtype TeletextPageType = TeletextPageType'
  { fromTeletextPageType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TeletextPageTypePageTypeInitial :: TeletextPageType
pattern TeletextPageTypePageTypeInitial = TeletextPageType' "PAGE_TYPE_INITIAL"

pattern TeletextPageTypePageTypeSubtitle :: TeletextPageType
pattern TeletextPageTypePageTypeSubtitle = TeletextPageType' "PAGE_TYPE_SUBTITLE"

pattern TeletextPageTypePageTypeAddlInfo :: TeletextPageType
pattern TeletextPageTypePageTypeAddlInfo = TeletextPageType' "PAGE_TYPE_ADDL_INFO"

pattern TeletextPageTypePageTypeProgramSchedule :: TeletextPageType
pattern TeletextPageTypePageTypeProgramSchedule = TeletextPageType' "PAGE_TYPE_PROGRAM_SCHEDULE"

pattern TeletextPageTypePageTypeHearingImpairedSubtitle :: TeletextPageType
pattern TeletextPageTypePageTypeHearingImpairedSubtitle = TeletextPageType' "PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE"

{-# COMPLETE
  TeletextPageTypePageTypeInitial,
  TeletextPageTypePageTypeSubtitle,
  TeletextPageTypePageTypeAddlInfo,
  TeletextPageTypePageTypeProgramSchedule,
  TeletextPageTypePageTypeHearingImpairedSubtitle,
  TeletextPageType'
  #-}
