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
-- Module      : Amazonka.MediaConvert.Types.BurninSubtitleApplyFontColor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BurninSubtitleApplyFontColor
  ( BurninSubtitleApplyFontColor
      ( ..,
        BurninSubtitleApplyFontColor_ALL_TEXT,
        BurninSubtitleApplyFontColor_WHITE_TEXT_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ignore this setting unless Style passthrough (StylePassthrough) is set
-- to Enabled and Font color (FontColor) set to Black, Yellow, Red, Green,
-- Blue, or Hex. Use Apply font color (ApplyFontColor) for additional font
-- color controls. When you choose White text only (WHITE_TEXT_ONLY), or
-- leave blank, your font color setting only applies to white text in your
-- input captions. For example, if your font color setting is Yellow, and
-- your input captions have red and white text, your output captions will
-- have red and yellow text. When you choose ALL_TEXT, your font color
-- setting applies to all of your output captions text.
newtype BurninSubtitleApplyFontColor = BurninSubtitleApplyFontColor'
  { fromBurninSubtitleApplyFontColor ::
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

pattern BurninSubtitleApplyFontColor_ALL_TEXT :: BurninSubtitleApplyFontColor
pattern BurninSubtitleApplyFontColor_ALL_TEXT = BurninSubtitleApplyFontColor' "ALL_TEXT"

pattern BurninSubtitleApplyFontColor_WHITE_TEXT_ONLY :: BurninSubtitleApplyFontColor
pattern BurninSubtitleApplyFontColor_WHITE_TEXT_ONLY = BurninSubtitleApplyFontColor' "WHITE_TEXT_ONLY"

{-# COMPLETE
  BurninSubtitleApplyFontColor_ALL_TEXT,
  BurninSubtitleApplyFontColor_WHITE_TEXT_ONLY,
  BurninSubtitleApplyFontColor'
  #-}
