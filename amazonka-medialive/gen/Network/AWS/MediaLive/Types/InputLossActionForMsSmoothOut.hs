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
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
  ( InputLossActionForMsSmoothOut
      ( ..,
        InputLossActionForMsSmoothOut_EMIT_OUTPUT,
        InputLossActionForMsSmoothOut_PAUSE_OUTPUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Input Loss Action For Ms Smooth Out
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut'
  { fromInputLossActionForMsSmoothOut ::
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

pattern InputLossActionForMsSmoothOut_EMIT_OUTPUT :: InputLossActionForMsSmoothOut
pattern InputLossActionForMsSmoothOut_EMIT_OUTPUT = InputLossActionForMsSmoothOut' "EMIT_OUTPUT"

pattern InputLossActionForMsSmoothOut_PAUSE_OUTPUT :: InputLossActionForMsSmoothOut
pattern InputLossActionForMsSmoothOut_PAUSE_OUTPUT = InputLossActionForMsSmoothOut' "PAUSE_OUTPUT"

{-# COMPLETE
  InputLossActionForMsSmoothOut_EMIT_OUTPUT,
  InputLossActionForMsSmoothOut_PAUSE_OUTPUT,
  InputLossActionForMsSmoothOut'
  #-}
