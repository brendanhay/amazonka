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
-- Module      : Amazonka.MediaLive.Types.InputLossActionForRtmpOut
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputLossActionForRtmpOut
  ( InputLossActionForRtmpOut
      ( ..,
        InputLossActionForRtmpOut_EMIT_OUTPUT,
        InputLossActionForRtmpOut_PAUSE_OUTPUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input Loss Action For Rtmp Out
newtype InputLossActionForRtmpOut = InputLossActionForRtmpOut'
  { fromInputLossActionForRtmpOut ::
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

pattern InputLossActionForRtmpOut_EMIT_OUTPUT :: InputLossActionForRtmpOut
pattern InputLossActionForRtmpOut_EMIT_OUTPUT = InputLossActionForRtmpOut' "EMIT_OUTPUT"

pattern InputLossActionForRtmpOut_PAUSE_OUTPUT :: InputLossActionForRtmpOut
pattern InputLossActionForRtmpOut_PAUSE_OUTPUT = InputLossActionForRtmpOut' "PAUSE_OUTPUT"

{-# COMPLETE
  InputLossActionForRtmpOut_EMIT_OUTPUT,
  InputLossActionForRtmpOut_PAUSE_OUTPUT,
  InputLossActionForRtmpOut'
  #-}
