{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.OperationStatus
  ( OperationStatus
      ( OperationStatus',
        OSPending,
        OSInProgress,
        OSSuccess,
        OSFailed
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

pattern OSPending :: OperationStatus
pattern OSPending = OperationStatus' "PENDING"

pattern OSInProgress :: OperationStatus
pattern OSInProgress = OperationStatus' "IN_PROGRESS"

pattern OSSuccess :: OperationStatus
pattern OSSuccess = OperationStatus' "SUCCESS"

pattern OSFailed :: OperationStatus
pattern OSFailed = OperationStatus' "FAILED"

{-# COMPLETE
  OSPending,
  OSInProgress,
  OSSuccess,
  OSFailed,
  OperationStatus'
  #-}
