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
        NotStarted,
        Started,
        Failed,
        Completed,
        Succeeded
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

pattern NotStarted :: OperationStatus
pattern NotStarted = OperationStatus' "NotStarted"

pattern Started :: OperationStatus
pattern Started = OperationStatus' "Started"

pattern Failed :: OperationStatus
pattern Failed = OperationStatus' "Failed"

pattern Completed :: OperationStatus
pattern Completed = OperationStatus' "Completed"

pattern Succeeded :: OperationStatus
pattern Succeeded = OperationStatus' "Succeeded"

{-# COMPLETE
  NotStarted,
  Started,
  Failed,
  Completed,
  Succeeded,
  OperationStatus'
  #-}
