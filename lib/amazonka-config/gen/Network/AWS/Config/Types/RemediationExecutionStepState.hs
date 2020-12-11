-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStepState
  ( RemediationExecutionStepState
      ( RemediationExecutionStepState',
        Failed,
        Pending,
        Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RemediationExecutionStepState = RemediationExecutionStepState' Lude.Text
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

pattern Failed :: RemediationExecutionStepState
pattern Failed = RemediationExecutionStepState' "FAILED"

pattern Pending :: RemediationExecutionStepState
pattern Pending = RemediationExecutionStepState' "PENDING"

pattern Succeeded :: RemediationExecutionStepState
pattern Succeeded = RemediationExecutionStepState' "SUCCEEDED"

{-# COMPLETE
  Failed,
  Pending,
  Succeeded,
  RemediationExecutionStepState'
  #-}
