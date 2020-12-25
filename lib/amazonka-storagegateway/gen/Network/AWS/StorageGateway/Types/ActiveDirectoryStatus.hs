{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ActiveDirectoryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ActiveDirectoryStatus
  ( ActiveDirectoryStatus
      ( ActiveDirectoryStatus',
        ActiveDirectoryStatusAccessDenied,
        ActiveDirectoryStatusDetached,
        ActiveDirectoryStatusJoined,
        ActiveDirectoryStatusJoining,
        ActiveDirectoryStatusNetworkError,
        ActiveDirectoryStatusTimeout,
        ActiveDirectoryStatusUnknownError,
        fromActiveDirectoryStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActiveDirectoryStatus = ActiveDirectoryStatus'
  { fromActiveDirectoryStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ActiveDirectoryStatusAccessDenied :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusAccessDenied = ActiveDirectoryStatus' "ACCESS_DENIED"

pattern ActiveDirectoryStatusDetached :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusDetached = ActiveDirectoryStatus' "DETACHED"

pattern ActiveDirectoryStatusJoined :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusJoined = ActiveDirectoryStatus' "JOINED"

pattern ActiveDirectoryStatusJoining :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusJoining = ActiveDirectoryStatus' "JOINING"

pattern ActiveDirectoryStatusNetworkError :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusNetworkError = ActiveDirectoryStatus' "NETWORK_ERROR"

pattern ActiveDirectoryStatusTimeout :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusTimeout = ActiveDirectoryStatus' "TIMEOUT"

pattern ActiveDirectoryStatusUnknownError :: ActiveDirectoryStatus
pattern ActiveDirectoryStatusUnknownError = ActiveDirectoryStatus' "UNKNOWN_ERROR"

{-# COMPLETE
  ActiveDirectoryStatusAccessDenied,
  ActiveDirectoryStatusDetached,
  ActiveDirectoryStatusJoined,
  ActiveDirectoryStatusJoining,
  ActiveDirectoryStatusNetworkError,
  ActiveDirectoryStatusTimeout,
  ActiveDirectoryStatusUnknownError,
  ActiveDirectoryStatus'
  #-}
