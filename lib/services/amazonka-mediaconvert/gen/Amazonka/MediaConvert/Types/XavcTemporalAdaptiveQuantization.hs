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
-- Module      : Amazonka.MediaConvert.Types.XavcTemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcTemporalAdaptiveQuantization
  ( XavcTemporalAdaptiveQuantization
      ( ..,
        XavcTemporalAdaptiveQuantization_DISABLED,
        XavcTemporalAdaptiveQuantization_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The best way to set up adaptive quantization is to keep the default
-- value, Auto (AUTO), for the setting Adaptive quantization
-- (adaptiveQuantization). When you do so, MediaConvert automatically
-- applies the best types of quantization for your video content. Include
-- this setting in your JSON job specification only when you choose to
-- change the default value for Adaptive quantization. For this setting,
-- keep the default value, Enabled (ENABLED), to adjust quantization within
-- each frame based on temporal variation of content complexity. When you
-- enable this feature, the encoder uses fewer bits on areas of the frame
-- that aren\'t moving and uses more bits on complex objects with sharp
-- edges that move a lot. For example, this feature improves the
-- readability of text tickers on newscasts and scoreboards on sports
-- matches. Enabling this feature will almost always improve your video
-- quality. Note, though, that this feature doesn\'t take into account
-- where the viewer\'s attention is likely to be. If viewers are likely to
-- be focusing their attention on a part of the screen that doesn\'t have
-- moving objects with sharp edges, such as sports athletes\' faces, you
-- might choose to disable this feature. Related setting: When you enable
-- temporal adaptive quantization, adjust the strength of the filter with
-- the setting Adaptive quantization (adaptiveQuantization).
newtype XavcTemporalAdaptiveQuantization = XavcTemporalAdaptiveQuantization'
  { fromXavcTemporalAdaptiveQuantization ::
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

pattern XavcTemporalAdaptiveQuantization_DISABLED :: XavcTemporalAdaptiveQuantization
pattern XavcTemporalAdaptiveQuantization_DISABLED = XavcTemporalAdaptiveQuantization' "DISABLED"

pattern XavcTemporalAdaptiveQuantization_ENABLED :: XavcTemporalAdaptiveQuantization
pattern XavcTemporalAdaptiveQuantization_ENABLED = XavcTemporalAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  XavcTemporalAdaptiveQuantization_DISABLED,
  XavcTemporalAdaptiveQuantization_ENABLED,
  XavcTemporalAdaptiveQuantization'
  #-}
