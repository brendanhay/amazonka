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
-- Module      : Amazonka.MediaLive.Types.InputDeviceConfiguredInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceConfiguredInput
  ( InputDeviceConfiguredInput
      ( ..,
        InputDeviceConfiguredInput_AUTO,
        InputDeviceConfiguredInput_HDMI,
        InputDeviceConfiguredInput_SDI
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The source to activate (use) from the input device.
newtype InputDeviceConfiguredInput = InputDeviceConfiguredInput'
  { fromInputDeviceConfiguredInput ::
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

pattern InputDeviceConfiguredInput_AUTO :: InputDeviceConfiguredInput
pattern InputDeviceConfiguredInput_AUTO = InputDeviceConfiguredInput' "AUTO"

pattern InputDeviceConfiguredInput_HDMI :: InputDeviceConfiguredInput
pattern InputDeviceConfiguredInput_HDMI = InputDeviceConfiguredInput' "HDMI"

pattern InputDeviceConfiguredInput_SDI :: InputDeviceConfiguredInput
pattern InputDeviceConfiguredInput_SDI = InputDeviceConfiguredInput' "SDI"

{-# COMPLETE
  InputDeviceConfiguredInput_AUTO,
  InputDeviceConfiguredInput_HDMI,
  InputDeviceConfiguredInput_SDI,
  InputDeviceConfiguredInput'
  #-}
