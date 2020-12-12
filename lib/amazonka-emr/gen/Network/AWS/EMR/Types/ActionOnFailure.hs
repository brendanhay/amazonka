{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ActionOnFailure
  ( ActionOnFailure
      ( ActionOnFailure',
        CancelAndWait,
        Continue,
        TerminateCluster,
        TerminateJobFlow
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionOnFailure = ActionOnFailure' Lude.Text
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

pattern CancelAndWait :: ActionOnFailure
pattern CancelAndWait = ActionOnFailure' "CANCEL_AND_WAIT"

pattern Continue :: ActionOnFailure
pattern Continue = ActionOnFailure' "CONTINUE"

pattern TerminateCluster :: ActionOnFailure
pattern TerminateCluster = ActionOnFailure' "TERMINATE_CLUSTER"

pattern TerminateJobFlow :: ActionOnFailure
pattern TerminateJobFlow = ActionOnFailure' "TERMINATE_JOB_FLOW"

{-# COMPLETE
  CancelAndWait,
  Continue,
  TerminateCluster,
  TerminateJobFlow,
  ActionOnFailure'
  #-}
