{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
  ( InputLossActionForMsSmoothOut
      ( InputLossActionForMsSmoothOut',
        EmitOutput,
        PauseOutput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input Loss Action For Ms Smooth Out
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut' Lude.Text
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

pattern EmitOutput :: InputLossActionForMsSmoothOut
pattern EmitOutput = InputLossActionForMsSmoothOut' "EMIT_OUTPUT"

pattern PauseOutput :: InputLossActionForMsSmoothOut
pattern PauseOutput = InputLossActionForMsSmoothOut' "PAUSE_OUTPUT"

{-# COMPLETE
  EmitOutput,
  PauseOutput,
  InputLossActionForMsSmoothOut'
  #-}
