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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
  ( DvbSubtitleShadowColor
      ( ..,
        DvbSubtitleShadowColor_BLACK,
        DvbSubtitleShadowColor_NONE,
        DvbSubtitleShadowColor_WHITE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor'
  { fromDvbSubtitleShadowColor ::
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

pattern DvbSubtitleShadowColor_BLACK :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_BLACK = DvbSubtitleShadowColor' "BLACK"

pattern DvbSubtitleShadowColor_NONE :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_NONE = DvbSubtitleShadowColor' "NONE"

pattern DvbSubtitleShadowColor_WHITE :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_WHITE = DvbSubtitleShadowColor' "WHITE"

{-# COMPLETE
  DvbSubtitleShadowColor_BLACK,
  DvbSubtitleShadowColor_NONE,
  DvbSubtitleShadowColor_WHITE,
  DvbSubtitleShadowColor'
  #-}
