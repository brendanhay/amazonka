{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPluginStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CommandPluginStatus
  ( CommandPluginStatus
      ( CommandPluginStatus',
        CPSPending,
        CPSInProgress,
        CPSSuccess,
        CPSTimedOut,
        CPSCancelled,
        CPSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CommandPluginStatus = CommandPluginStatus' Lude.Text
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

pattern CPSPending :: CommandPluginStatus
pattern CPSPending = CommandPluginStatus' "Pending"

pattern CPSInProgress :: CommandPluginStatus
pattern CPSInProgress = CommandPluginStatus' "InProgress"

pattern CPSSuccess :: CommandPluginStatus
pattern CPSSuccess = CommandPluginStatus' "Success"

pattern CPSTimedOut :: CommandPluginStatus
pattern CPSTimedOut = CommandPluginStatus' "TimedOut"

pattern CPSCancelled :: CommandPluginStatus
pattern CPSCancelled = CommandPluginStatus' "Cancelled"

pattern CPSFailed :: CommandPluginStatus
pattern CPSFailed = CommandPluginStatus' "Failed"

{-# COMPLETE
  CPSPending,
  CPSInProgress,
  CPSSuccess,
  CPSTimedOut,
  CPSCancelled,
  CPSFailed,
  CommandPluginStatus'
  #-}
