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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleApplyFontColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleApplyFontColor
  ( DvbSubtitleApplyFontColor
      ( ..,
        DvbSubtitleApplyFontColor_ALL_TEXT,
        DvbSubtitleApplyFontColor_WHITE_TEXT_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Ignore this setting unless your input captions are STL, any type of 608,
-- teletext, or TTML, and your output captions are DVB-SUB. Specify how the
-- service applies the color specified in the setting Font color
-- (DvbSubtitleFontColor). By default, this color is white. When you choose
-- WHITE_TEXT_ONLY, the service uses the specified font color only for text
-- that is white in the input. When you choose ALL_TEXT, the service uses
-- the specified font color for all output captions text. If you leave both
-- settings at their default value, your output font color is the same as
-- your input font color.
newtype DvbSubtitleApplyFontColor = DvbSubtitleApplyFontColor'
  { fromDvbSubtitleApplyFontColor ::
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

pattern DvbSubtitleApplyFontColor_ALL_TEXT :: DvbSubtitleApplyFontColor
pattern DvbSubtitleApplyFontColor_ALL_TEXT = DvbSubtitleApplyFontColor' "ALL_TEXT"

pattern DvbSubtitleApplyFontColor_WHITE_TEXT_ONLY :: DvbSubtitleApplyFontColor
pattern DvbSubtitleApplyFontColor_WHITE_TEXT_ONLY = DvbSubtitleApplyFontColor' "WHITE_TEXT_ONLY"

{-# COMPLETE
  DvbSubtitleApplyFontColor_ALL_TEXT,
  DvbSubtitleApplyFontColor_WHITE_TEXT_ONLY,
  DvbSubtitleApplyFontColor'
  #-}
