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
        HCLSInsert,
        HCLSOmit,
        HCLSNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting' Lude.Text
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

pattern HCLSInsert :: HlsCaptionLanguageSetting
pattern HCLSInsert = HlsCaptionLanguageSetting' "INSERT"

pattern HCLSOmit :: HlsCaptionLanguageSetting
pattern HCLSOmit = HlsCaptionLanguageSetting' "OMIT"

pattern HCLSNone :: HlsCaptionLanguageSetting
pattern HCLSNone = HlsCaptionLanguageSetting' "NONE"

{-# COMPLETE
  HCLSInsert,
  HCLSOmit,
  HCLSNone,
  HlsCaptionLanguageSetting'
  #-}
