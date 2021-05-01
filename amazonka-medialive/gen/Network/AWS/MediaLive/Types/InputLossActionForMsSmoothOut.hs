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

import qualified Network.AWS.Prelude as Prelude

-- | Input Loss Action For Ms Smooth Out
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut'
  { fromInputLossActionForMsSmoothOut ::
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

pattern InputLossActionForMsSmoothOut_EMIT_OUTPUT :: InputLossActionForMsSmoothOut
pattern InputLossActionForMsSmoothOut_EMIT_OUTPUT = InputLossActionForMsSmoothOut' "EMIT_OUTPUT"

pattern InputLossActionForMsSmoothOut_PAUSE_OUTPUT :: InputLossActionForMsSmoothOut
pattern InputLossActionForMsSmoothOut_PAUSE_OUTPUT = InputLossActionForMsSmoothOut' "PAUSE_OUTPUT"

{-# COMPLETE
  InputLossActionForMsSmoothOut_EMIT_OUTPUT,
  InputLossActionForMsSmoothOut_PAUSE_OUTPUT,
  InputLossActionForMsSmoothOut'
  #-}
