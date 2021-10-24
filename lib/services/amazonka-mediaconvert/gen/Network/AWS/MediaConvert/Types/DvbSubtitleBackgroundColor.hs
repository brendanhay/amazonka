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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
  ( DvbSubtitleBackgroundColor
      ( ..,
        DvbSubtitleBackgroundColor_AUTO,
        DvbSubtitleBackgroundColor_BLACK,
        DvbSubtitleBackgroundColor_NONE,
        DvbSubtitleBackgroundColor_WHITE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the color of the rectangle behind the captions. Leave background
-- color (BackgroundColor) blank and set Style passthrough
-- (StylePassthrough) to enabled to use the background color data from your
-- input captions, if present.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor'
  { fromDvbSubtitleBackgroundColor ::
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

pattern DvbSubtitleBackgroundColor_AUTO :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_AUTO = DvbSubtitleBackgroundColor' "AUTO"

pattern DvbSubtitleBackgroundColor_BLACK :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_BLACK = DvbSubtitleBackgroundColor' "BLACK"

pattern DvbSubtitleBackgroundColor_NONE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_NONE = DvbSubtitleBackgroundColor' "NONE"

pattern DvbSubtitleBackgroundColor_WHITE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_WHITE = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  DvbSubtitleBackgroundColor_AUTO,
  DvbSubtitleBackgroundColor_BLACK,
  DvbSubtitleBackgroundColor_NONE,
  DvbSubtitleBackgroundColor_WHITE,
  DvbSubtitleBackgroundColor'
  #-}
