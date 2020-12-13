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
        AOFTerminateJobFlow,
        AOFTerminateCluster,
        AOFCancelAndWait,
        AOFContinue
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

pattern AOFTerminateJobFlow :: ActionOnFailure
pattern AOFTerminateJobFlow = ActionOnFailure' "TERMINATE_JOB_FLOW"

pattern AOFTerminateCluster :: ActionOnFailure
pattern AOFTerminateCluster = ActionOnFailure' "TERMINATE_CLUSTER"

pattern AOFCancelAndWait :: ActionOnFailure
pattern AOFCancelAndWait = ActionOnFailure' "CANCEL_AND_WAIT"

pattern AOFContinue :: ActionOnFailure
pattern AOFContinue = ActionOnFailure' "CONTINUE"

{-# COMPLETE
  AOFTerminateJobFlow,
  AOFTerminateCluster,
  AOFCancelAndWait,
  AOFContinue,
  ActionOnFailure'
  #-}
