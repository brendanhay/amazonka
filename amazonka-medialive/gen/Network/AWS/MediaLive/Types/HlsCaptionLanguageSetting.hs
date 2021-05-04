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
-- Module      : Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsCaptionLanguageSetting
  ( HlsCaptionLanguageSetting
      ( ..,
        HlsCaptionLanguageSetting_INSERT,
        HlsCaptionLanguageSetting_NONE,
        HlsCaptionLanguageSetting_OMIT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Caption Language Setting
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
