{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SignalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SignalType
  ( SignalType
      ( SignalType',
        SignalTypeApprove,
        SignalTypeReject,
        SignalTypeStartStep,
        SignalTypeStopStep,
        SignalTypeResume,
        fromSignalType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SignalType = SignalType' {fromSignalType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SignalTypeApprove :: SignalType
pattern SignalTypeApprove = SignalType' "Approve"

pattern SignalTypeReject :: SignalType
pattern SignalTypeReject = SignalType' "Reject"

pattern SignalTypeStartStep :: SignalType
pattern SignalTypeStartStep = SignalType' "StartStep"

pattern SignalTypeStopStep :: SignalType
pattern SignalTypeStopStep = SignalType' "StopStep"

pattern SignalTypeResume :: SignalType
pattern SignalTypeResume = SignalType' "Resume"

{-# COMPLETE
  SignalTypeApprove,
  SignalTypeReject,
  SignalTypeStartStep,
  SignalTypeStopStep,
  SignalTypeResume,
  SignalType'
  #-}
