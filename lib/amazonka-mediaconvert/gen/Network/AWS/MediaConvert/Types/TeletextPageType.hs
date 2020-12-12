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
        PageTypeAddlInfo,
        PageTypeHearingImpairedSubtitle,
        PageTypeInitial,
        PageTypeProgramSchedule,
        PageTypeSubtitle
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
newtype TeletextPageType = TeletextPageType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PageTypeAddlInfo :: TeletextPageType
pattern PageTypeAddlInfo = TeletextPageType' "PAGE_TYPE_ADDL_INFO"

pattern PageTypeHearingImpairedSubtitle :: TeletextPageType
pattern PageTypeHearingImpairedSubtitle = TeletextPageType' "PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE"

pattern PageTypeInitial :: TeletextPageType
pattern PageTypeInitial = TeletextPageType' "PAGE_TYPE_INITIAL"

pattern PageTypeProgramSchedule :: TeletextPageType
pattern PageTypeProgramSchedule = TeletextPageType' "PAGE_TYPE_PROGRAM_SCHEDULE"

pattern PageTypeSubtitle :: TeletextPageType
pattern PageTypeSubtitle = TeletextPageType' "PAGE_TYPE_SUBTITLE"

{-# COMPLETE
  PageTypeAddlInfo,
  PageTypeHearingImpairedSubtitle,
  PageTypeInitial,
  PageTypeProgramSchedule,
  PageTypeSubtitle,
  TeletextPageType'
  #-}
