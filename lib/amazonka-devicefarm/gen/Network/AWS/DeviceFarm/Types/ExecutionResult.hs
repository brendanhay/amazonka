{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionResult
  ( ExecutionResult
      ( ExecutionResult',
        ERPending,
        ERPassed,
        ERWarned,
        ERFailed,
        ERSkipped,
        ERErrored,
        ERStopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExecutionResult = ExecutionResult' Lude.Text
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

pattern ERPending :: ExecutionResult
pattern ERPending = ExecutionResult' "PENDING"

pattern ERPassed :: ExecutionResult
pattern ERPassed = ExecutionResult' "PASSED"

pattern ERWarned :: ExecutionResult
pattern ERWarned = ExecutionResult' "WARNED"

pattern ERFailed :: ExecutionResult
pattern ERFailed = ExecutionResult' "FAILED"

pattern ERSkipped :: ExecutionResult
pattern ERSkipped = ExecutionResult' "SKIPPED"

pattern ERErrored :: ExecutionResult
pattern ERErrored = ExecutionResult' "ERRORED"

pattern ERStopped :: ExecutionResult
pattern ERStopped = ExecutionResult' "STOPPED"

{-# COMPLETE
  ERPending,
  ERPassed,
  ERWarned,
  ERFailed,
  ERSkipped,
  ERErrored,
  ERStopped,
  ExecutionResult'
  #-}
