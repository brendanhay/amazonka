{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( ..,
        HlsCaptionLanguageSetting_INSERT,
        HlsCaptionLanguageSetting_NONE,
        HlsCaptionLanguageSetting_OMIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
