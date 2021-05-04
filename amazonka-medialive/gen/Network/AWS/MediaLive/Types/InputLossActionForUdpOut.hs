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
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForUdpOut
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForUdpOut
  ( InputLossActionForUdpOut
      ( ..,
        InputLossActionForUdpOut_DROP_PROGRAM,
        InputLossActionForUdpOut_DROP_TS,
        InputLossActionForUdpOut_EMIT_PROGRAM
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Input Loss Action For Udp Out
newtype InputLossActionForUdpOut = InputLossActionForUdpOut'
  { fromInputLossActionForUdpOut ::
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

pattern InputLossActionForUdpOut_DROP_PROGRAM :: InputLossActionForUdpOut
pattern InputLossActionForUdpOut_DROP_PROGRAM = InputLossActionForUdpOut' "DROP_PROGRAM"

pattern InputLossActionForUdpOut_DROP_TS :: InputLossActionForUdpOut
pattern InputLossActionForUdpOut_DROP_TS = InputLossActionForUdpOut' "DROP_TS"

pattern InputLossActionForUdpOut_EMIT_PROGRAM :: InputLossActionForUdpOut
pattern InputLossActionForUdpOut_EMIT_PROGRAM = InputLossActionForUdpOut' "EMIT_PROGRAM"

{-# COMPLETE
  InputLossActionForUdpOut_DROP_PROGRAM,
  InputLossActionForUdpOut_DROP_TS,
  InputLossActionForUdpOut_EMIT_PROGRAM,
  InputLossActionForUdpOut'
  #-}
