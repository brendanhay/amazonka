-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForHlsOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForHlsOut
  ( InputLossActionForHlsOut
      ( InputLossActionForHlsOut',
        ILAFHOEmitOutput,
        ILAFHOPauseOutput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input Loss Action For Hls Out
newtype InputLossActionForHlsOut = InputLossActionForHlsOut' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ILAFHOEmitOutput :: InputLossActionForHlsOut
pattern ILAFHOEmitOutput = InputLossActionForHlsOut' "EMIT_OUTPUT"

pattern ILAFHOPauseOutput :: InputLossActionForHlsOut
pattern ILAFHOPauseOutput = InputLossActionForHlsOut' "PAUSE_OUTPUT"

{-# COMPLETE
  ILAFHOEmitOutput,
  ILAFHOPauseOutput,
  InputLossActionForHlsOut'
  #-}
