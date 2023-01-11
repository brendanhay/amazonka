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
-- Module      : Amazonka.MediaLive.Types.InputLossActionForHlsOut
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputLossActionForHlsOut
  ( InputLossActionForHlsOut
      ( ..,
        InputLossActionForHlsOut_EMIT_OUTPUT,
        InputLossActionForHlsOut_PAUSE_OUTPUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input Loss Action For Hls Out
newtype InputLossActionForHlsOut = InputLossActionForHlsOut'
  { fromInputLossActionForHlsOut ::
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

pattern InputLossActionForHlsOut_EMIT_OUTPUT :: InputLossActionForHlsOut
pattern InputLossActionForHlsOut_EMIT_OUTPUT = InputLossActionForHlsOut' "EMIT_OUTPUT"

pattern InputLossActionForHlsOut_PAUSE_OUTPUT :: InputLossActionForHlsOut
pattern InputLossActionForHlsOut_PAUSE_OUTPUT = InputLossActionForHlsOut' "PAUSE_OUTPUT"

{-# COMPLETE
  InputLossActionForHlsOut_EMIT_OUTPUT,
  InputLossActionForHlsOut_PAUSE_OUTPUT,
  InputLossActionForHlsOut'
  #-}
