{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftStatus
  ( StackResourceDriftStatus
      ( StackResourceDriftStatus',
        SRDSDeleted,
        SRDSInSync,
        SRDSModified,
        SRDSNotChecked
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackResourceDriftStatus = StackResourceDriftStatus' Lude.Text
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

pattern SRDSDeleted :: StackResourceDriftStatus
pattern SRDSDeleted = StackResourceDriftStatus' "DELETED"

pattern SRDSInSync :: StackResourceDriftStatus
pattern SRDSInSync = StackResourceDriftStatus' "IN_SYNC"

pattern SRDSModified :: StackResourceDriftStatus
pattern SRDSModified = StackResourceDriftStatus' "MODIFIED"

pattern SRDSNotChecked :: StackResourceDriftStatus
pattern SRDSNotChecked = StackResourceDriftStatus' "NOT_CHECKED"

{-# COMPLETE
  SRDSDeleted,
  SRDSInSync,
  SRDSModified,
  SRDSNotChecked,
  StackResourceDriftStatus'
  #-}
