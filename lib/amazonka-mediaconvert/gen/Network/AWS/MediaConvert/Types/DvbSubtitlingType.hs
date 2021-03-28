{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitlingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitlingType
  ( DvbSubtitlingType
    ( DvbSubtitlingType'
    , DvbSubtitlingTypeHearingImpaired
    , DvbSubtitlingTypeStandard
    , fromDvbSubtitlingType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
newtype DvbSubtitlingType = DvbSubtitlingType'{fromDvbSubtitlingType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern DvbSubtitlingTypeHearingImpaired :: DvbSubtitlingType
pattern DvbSubtitlingTypeHearingImpaired = DvbSubtitlingType' "HEARING_IMPAIRED"

pattern DvbSubtitlingTypeStandard :: DvbSubtitlingType
pattern DvbSubtitlingTypeStandard = DvbSubtitlingType' "STANDARD"

{-# COMPLETE 
  DvbSubtitlingTypeHearingImpaired,

  DvbSubtitlingTypeStandard,
  DvbSubtitlingType'
  #-}
