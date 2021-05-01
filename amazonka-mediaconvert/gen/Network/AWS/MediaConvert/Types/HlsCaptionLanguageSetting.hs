{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( ..,
        HlsCaptionLanguageSetting_INSERT,
        HlsCaptionLanguageSetting_NONE,
        HlsCaptionLanguageSetting_OMIT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Applies only to 608 Embedded output captions. Insert: Include
-- CLOSED-CAPTIONS lines in the manifest. Specify at least one language in
-- the CC1 Language Code field. One CLOSED-CAPTION line is added for each
-- Language Code you specify. Make sure to specify the languages in the
-- order in which they appear in the original source (if the source is
-- embedded format) or the order of the caption selectors (if the source is
-- other than embedded). Otherwise, languages in the manifest will not
-- match up properly with the output captions. None: Include
-- CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any
-- CLOSED-CAPTIONS line from the manifest.
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting'
  { fromHlsCaptionLanguageSetting ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern HlsCaptionLanguageSetting_INSERT :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSetting_INSERT = HlsCaptionLanguageSetting' "INSERT"

pattern HlsCaptionLanguageSetting_NONE :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSetting_NONE = HlsCaptionLanguageSetting' "NONE"

pattern HlsCaptionLanguageSetting_OMIT :: HlsCaptionLanguageSetting
pattern HlsCaptionLanguageSetting_OMIT = HlsCaptionLanguageSetting' "OMIT"

{-# COMPLETE
  HlsCaptionLanguageSetting_INSERT,
  HlsCaptionLanguageSetting_NONE,
  HlsCaptionLanguageSetting_OMIT,
  HlsCaptionLanguageSetting'
  #-}
