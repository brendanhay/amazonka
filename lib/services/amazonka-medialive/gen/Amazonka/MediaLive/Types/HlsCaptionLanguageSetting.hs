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
-- Module      : Amazonka.MediaLive.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( ..,
        HlsCaptionLanguageSetting_INSERT,
        HlsCaptionLanguageSetting_NONE,
        HlsCaptionLanguageSetting_OMIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Hls Caption Language Setting
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting'
  { fromHlsCaptionLanguageSetting ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
