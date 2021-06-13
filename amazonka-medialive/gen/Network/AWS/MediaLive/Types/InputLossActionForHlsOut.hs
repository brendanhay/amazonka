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
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForHlsOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForHlsOut
  ( InputLossActionForHlsOut
      ( ..,
        InputLossActionForHlsOut_EMIT_OUTPUT,
        InputLossActionForHlsOut_PAUSE_OUTPUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Input Loss Action For Hls Out
newtype InputLossActionForHlsOut = InputLossActionForHlsOut'
  { fromInputLossActionForHlsOut ::
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

pattern InputLossActionForHlsOut_EMIT_OUTPUT :: InputLossActionForHlsOut
pattern InputLossActionForHlsOut_EMIT_OUTPUT = InputLossActionForHlsOut' "EMIT_OUTPUT"

pattern InputLossActionForHlsOut_PAUSE_OUTPUT :: InputLossActionForHlsOut
pattern InputLossActionForHlsOut_PAUSE_OUTPUT = InputLossActionForHlsOut' "PAUSE_OUTPUT"

{-# COMPLETE
  InputLossActionForHlsOut_EMIT_OUTPUT,
  InputLossActionForHlsOut_PAUSE_OUTPUT,
  InputLossActionForHlsOut'
  #-}
