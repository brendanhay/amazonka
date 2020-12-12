{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetStatus
  ( ChangeSetStatus
      ( ChangeSetStatus',
        CSSCreateComplete,
        CSSCreateInProgress,
        CSSCreatePending,
        CSSDeleteComplete,
        CSSDeleteFailed,
        CSSDeleteInProgress,
        CSSDeletePending,
        CSSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChangeSetStatus = ChangeSetStatus' Lude.Text
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

pattern CSSCreateComplete :: ChangeSetStatus
pattern CSSCreateComplete = ChangeSetStatus' "CREATE_COMPLETE"

pattern CSSCreateInProgress :: ChangeSetStatus
pattern CSSCreateInProgress = ChangeSetStatus' "CREATE_IN_PROGRESS"

pattern CSSCreatePending :: ChangeSetStatus
pattern CSSCreatePending = ChangeSetStatus' "CREATE_PENDING"

pattern CSSDeleteComplete :: ChangeSetStatus
pattern CSSDeleteComplete = ChangeSetStatus' "DELETE_COMPLETE"

pattern CSSDeleteFailed :: ChangeSetStatus
pattern CSSDeleteFailed = ChangeSetStatus' "DELETE_FAILED"

pattern CSSDeleteInProgress :: ChangeSetStatus
pattern CSSDeleteInProgress = ChangeSetStatus' "DELETE_IN_PROGRESS"

pattern CSSDeletePending :: ChangeSetStatus
pattern CSSDeletePending = ChangeSetStatus' "DELETE_PENDING"

pattern CSSFailed :: ChangeSetStatus
pattern CSSFailed = ChangeSetStatus' "FAILED"

{-# COMPLETE
  CSSCreateComplete,
  CSSCreateInProgress,
  CSSCreatePending,
  CSSDeleteComplete,
  CSSDeleteFailed,
  CSSDeleteInProgress,
  CSSDeletePending,
  CSSFailed,
  ChangeSetStatus'
  #-}
