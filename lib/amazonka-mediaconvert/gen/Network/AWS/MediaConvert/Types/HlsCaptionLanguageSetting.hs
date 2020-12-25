{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( HlsCaptionLanguageSetting',
        HlsCaptionLanguageSettingInsert,
        HlsCaptionLanguageSettingOmit,
        HlsCaptionLanguageSettingNone,
        fromHlsCaptionLanguageSetting
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
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

pattern HlsCaptionLanguageSettingOmit :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSettingOmit = HlsCaptionLanguageSetting' "OMIT"

pattern HlsCaptionLanguageSettingNone :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSettingNone = HlsCaptionLanguageSetting' "NONE"

{-# COMPLETE
  HlsCaptionLanguageSettingInsert,
  HlsCaptionLanguageSettingOmit,
  HlsCaptionLanguageSettingNone,
  HlsCaptionLanguageSetting'
  #-}
