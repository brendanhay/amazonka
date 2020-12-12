{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.OperationStatus
  ( OperationStatus
      ( OperationStatus',
        OSCompleted,
        OSFailed,
        OSNotStarted,
        OSStarted,
        OSSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperationStatus = OperationStatus' Lude.Text
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

pattern OSCompleted :: OperationStatus
pattern OSCompleted = OperationStatus' "Completed"

pattern OSFailed :: OperationStatus
pattern OSFailed = OperationStatus' "Failed"

pattern OSNotStarted :: OperationStatus
pattern OSNotStarted = OperationStatus' "NotStarted"

pattern OSStarted :: OperationStatus
pattern OSStarted = OperationStatus' "Started"

pattern OSSucceeded :: OperationStatus
pattern OSSucceeded = OperationStatus' "Succeeded"

{-# COMPLETE
  OSCompleted,
  OSFailed,
  OSNotStarted,
  OSStarted,
  OSSucceeded,
  OperationStatus'
  #-}
