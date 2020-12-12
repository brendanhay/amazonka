{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerState
  ( BrokerState
      ( BrokerState',
        CreationFailed,
        CreationInProgress,
        DeletionInProgress,
        RebootInProgress,
        Running
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The status of the broker.
newtype BrokerState = BrokerState' Lude.Text
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

pattern CreationFailed :: BrokerState
pattern CreationFailed = BrokerState' "CREATION_FAILED"

pattern CreationInProgress :: BrokerState
pattern CreationInProgress = BrokerState' "CREATION_IN_PROGRESS"

pattern DeletionInProgress :: BrokerState
pattern DeletionInProgress = BrokerState' "DELETION_IN_PROGRESS"

pattern RebootInProgress :: BrokerState
pattern RebootInProgress = BrokerState' "REBOOT_IN_PROGRESS"

pattern Running :: BrokerState
pattern Running = BrokerState' "RUNNING"

{-# COMPLETE
  CreationFailed,
  CreationInProgress,
  DeletionInProgress,
  RebootInProgress,
  Running,
  BrokerState'
  #-}
