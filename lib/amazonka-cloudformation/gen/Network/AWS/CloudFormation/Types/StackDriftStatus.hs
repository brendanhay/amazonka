{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftStatus
  ( StackDriftStatus
      ( StackDriftStatus',
        SDSDrifted,
        SDSInSync,
        SDSNotChecked,
        SDSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackDriftStatus = StackDriftStatus' Lude.Text
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

pattern SDSDrifted :: StackDriftStatus
pattern SDSDrifted = StackDriftStatus' "DRIFTED"

pattern SDSInSync :: StackDriftStatus
pattern SDSInSync = StackDriftStatus' "IN_SYNC"

pattern SDSNotChecked :: StackDriftStatus
pattern SDSNotChecked = StackDriftStatus' "NOT_CHECKED"

pattern SDSUnknown :: StackDriftStatus
pattern SDSUnknown = StackDriftStatus' "UNKNOWN"

{-# COMPLETE
  SDSDrifted,
  SDSInSync,
  SDSNotChecked,
  SDSUnknown,
  StackDriftStatus'
  #-}
