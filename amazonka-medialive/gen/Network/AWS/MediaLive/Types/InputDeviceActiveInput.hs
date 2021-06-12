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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceActiveInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceActiveInput
  ( InputDeviceActiveInput
      ( ..,
        InputDeviceActiveInput_HDMI,
        InputDeviceActiveInput_SDI
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The source at the input device that is currently active.
newtype InputDeviceActiveInput = InputDeviceActiveInput'
  { fromInputDeviceActiveInput ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern InputDeviceActiveInput_HDMI :: InputDeviceActiveInput
pattern InputDeviceActiveInput_HDMI = InputDeviceActiveInput' "HDMI"

pattern InputDeviceActiveInput_SDI :: InputDeviceActiveInput
pattern InputDeviceActiveInput_SDI = InputDeviceActiveInput' "SDI"

{-# COMPLETE
  InputDeviceActiveInput_HDMI,
  InputDeviceActiveInput_SDI,
  InputDeviceActiveInput'
  #-}
