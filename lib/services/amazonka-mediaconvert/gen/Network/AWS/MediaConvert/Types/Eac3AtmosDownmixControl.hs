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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDownmixControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDownmixControl
  ( Eac3AtmosDownmixControl
      ( ..,
        Eac3AtmosDownmixControl_INITIALIZE_FROM_SOURCE,
        Eac3AtmosDownmixControl_SPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify whether MediaConvert should use any downmix metadata from your
-- input file. Keep the default value, Custom (SPECIFIED) to provide
-- downmix values in your job settings. Choose Follow source
-- (INITIALIZE_FROM_SOURCE) to use the metadata from your input. Related
-- settings--Use these settings to specify your downmix values: Left
-- only\/Right only surround (LoRoSurroundMixLevel), Left total\/Right
-- total surround (LtRtSurroundMixLevel), Left total\/Right total center
-- (LtRtCenterMixLevel), Left only\/Right only center (LoRoCenterMixLevel),
-- and Stereo downmix (StereoDownmix). When you keep Custom (SPECIFIED) for
-- Downmix control (DownmixControl) and you don\'t specify values for the
-- related settings, MediaConvert uses default values for those settings.
newtype Eac3AtmosDownmixControl = Eac3AtmosDownmixControl'
  { fromEac3AtmosDownmixControl ::
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

pattern Eac3AtmosDownmixControl_INITIALIZE_FROM_SOURCE :: Eac3AtmosDownmixControl
pattern Eac3AtmosDownmixControl_INITIALIZE_FROM_SOURCE = Eac3AtmosDownmixControl' "INITIALIZE_FROM_SOURCE"

pattern Eac3AtmosDownmixControl_SPECIFIED :: Eac3AtmosDownmixControl
pattern Eac3AtmosDownmixControl_SPECIFIED = Eac3AtmosDownmixControl' "SPECIFIED"

{-# COMPLETE
  Eac3AtmosDownmixControl_INITIALIZE_FROM_SOURCE,
  Eac3AtmosDownmixControl_SPECIFIED,
  Eac3AtmosDownmixControl'
  #-}
