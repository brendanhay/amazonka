{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationStatus
  ( OperationStatus
      ( OperationStatus',
        Error,
        Failed,
        InProgress,
        Submitted,
        Successful
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

pattern Error :: OperationStatus
pattern Error = OperationStatus' "ERROR"

pattern Failed :: OperationStatus
pattern Failed = OperationStatus' "FAILED"

pattern InProgress :: OperationStatus
pattern InProgress = OperationStatus' "IN_PROGRESS"

pattern Submitted :: OperationStatus
pattern Submitted = OperationStatus' "SUBMITTED"

pattern Successful :: OperationStatus
pattern Successful = OperationStatus' "SUCCESSFUL"

{-# COMPLETE
  Error,
  Failed,
  InProgress,
  Submitted,
  Successful,
  OperationStatus'
  #-}
