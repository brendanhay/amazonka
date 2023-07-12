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
-- Module      : Amazonka.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbSubtitleShadowColor
  ( DvbSubtitleShadowColor
      ( ..,
        DvbSubtitleShadowColor_AUTO,
        DvbSubtitleShadowColor_BLACK,
        DvbSubtitleShadowColor_NONE,
        DvbSubtitleShadowColor_WHITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present. Within your job settings, all of your DVB-Sub settings must be
-- identical.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor'
  { fromDvbSubtitleShadowColor ::
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

pattern DvbSubtitleShadowColor_AUTO :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_AUTO = DvbSubtitleShadowColor' "AUTO"

pattern DvbSubtitleShadowColor_BLACK :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_BLACK = DvbSubtitleShadowColor' "BLACK"

pattern DvbSubtitleShadowColor_NONE :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_NONE = DvbSubtitleShadowColor' "NONE"

pattern DvbSubtitleShadowColor_WHITE :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColor_WHITE = DvbSubtitleShadowColor' "WHITE"

{-# COMPLETE
  DvbSubtitleShadowColor_AUTO,
  DvbSubtitleShadowColor_BLACK,
  DvbSubtitleShadowColor_NONE,
  DvbSubtitleShadowColor_WHITE,
  DvbSubtitleShadowColor'
  #-}
