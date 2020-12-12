{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
  ( TrialComponentPrimaryStatus
      ( TrialComponentPrimaryStatus',
        TCPSCompleted,
        TCPSFailed,
        TCPSInProgress,
        TCPSStopped,
        TCPSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrialComponentPrimaryStatus = TrialComponentPrimaryStatus' Lude.Text
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

pattern TCPSCompleted :: TrialComponentPrimaryStatus
pattern TCPSCompleted = TrialComponentPrimaryStatus' "Completed"

pattern TCPSFailed :: TrialComponentPrimaryStatus
pattern TCPSFailed = TrialComponentPrimaryStatus' "Failed"

pattern TCPSInProgress :: TrialComponentPrimaryStatus
pattern TCPSInProgress = TrialComponentPrimaryStatus' "InProgress"

pattern TCPSStopped :: TrialComponentPrimaryStatus
pattern TCPSStopped = TrialComponentPrimaryStatus' "Stopped"

pattern TCPSStopping :: TrialComponentPrimaryStatus
pattern TCPSStopping = TrialComponentPrimaryStatus' "Stopping"

{-# COMPLETE
  TCPSCompleted,
  TCPSFailed,
  TCPSInProgress,
  TCPSStopped,
  TCPSStopping,
  TrialComponentPrimaryStatus'
  #-}
