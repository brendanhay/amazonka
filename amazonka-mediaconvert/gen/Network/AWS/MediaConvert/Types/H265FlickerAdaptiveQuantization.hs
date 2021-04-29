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
-- Module      : Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
  ( H265FlickerAdaptiveQuantization
      ( ..,
        H265FlickerAdaptiveQuantization_DISABLED,
        H265FlickerAdaptiveQuantization_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop
-- appears as a visual flicker that can arise when the encoder saves bits
-- by copying some macroblocks many times from frame to frame, and then
-- refreshes them at the I-frame. When you enable this setting, the encoder
-- updates these macroblocks slightly more often to smooth out the flicker.
-- This setting is disabled by default. Related setting: In addition to
-- enabling this setting, you must also set adaptiveQuantization to a value
-- other than Off (OFF).
newtype H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization'
  { fromH265FlickerAdaptiveQuantization ::
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

pattern H265FlickerAdaptiveQuantization_DISABLED :: H265FlickerAdaptiveQuantization
pattern H265FlickerAdaptiveQuantization_DISABLED = H265FlickerAdaptiveQuantization' "DISABLED"

pattern H265FlickerAdaptiveQuantization_ENABLED :: H265FlickerAdaptiveQuantization
pattern H265FlickerAdaptiveQuantization_ENABLED = H265FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H265FlickerAdaptiveQuantization_DISABLED,
  H265FlickerAdaptiveQuantization_ENABLED,
  H265FlickerAdaptiveQuantization'
  #-}
