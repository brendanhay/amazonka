{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitlingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitlingType
  ( DvbSubtitlingType
      ( DvbSubtitlingType',
        HearingImpaired,
        Standard
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether your DVB subtitles are standard or for hearing impaired. Choose hearing impaired if your subtitles include audio descriptions and dialogue. Choose standard if your subtitles include only dialogue.
newtype DvbSubtitlingType = DvbSubtitlingType' Lude.Text
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

pattern HearingImpaired :: DvbSubtitlingType
pattern HearingImpaired = DvbSubtitlingType' "HEARING_IMPAIRED"

pattern Standard :: DvbSubtitlingType
pattern Standard = DvbSubtitlingType' "STANDARD"

{-# COMPLETE
  HearingImpaired,
  Standard,
  DvbSubtitlingType'
  #-}
