{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( HlsCaptionLanguageSetting',
        HlsCaptionLanguageSettingInsert,
        HlsCaptionLanguageSettingNone,
        HlsCaptionLanguageSettingOmit,
        fromHlsCaptionLanguageSetting
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Hls Caption Language Setting
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting'
  { fromHlsCaptionLanguageSetting ::
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

pattern HlsCaptionLanguageSettingInsert :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSettingInsert = HlsCaptionLanguageSetting' "INSERT"

pattern HlsCaptionLanguageSettingNone :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSettingNone = HlsCaptionLanguageSetting' "NONE"

pattern HlsCaptionLanguageSettingOmit :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSettingOmit = HlsCaptionLanguageSetting' "OMIT"

{-# COMPLETE
  HlsCaptionLanguageSettingInsert,
  HlsCaptionLanguageSettingNone,
  HlsCaptionLanguageSettingOmit,
  HlsCaptionLanguageSetting'
  #-}
