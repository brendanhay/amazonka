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
        AccessDenied,
        Detached,
        Joined,
        Joining,
        NetworkError,
        Timeout,
        UnknownError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActiveDirectoryStatus = ActiveDirectoryStatus' Lude.Text
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

pattern AccessDenied :: ActiveDirectoryStatus
pattern AccessDenied = ActiveDirectoryStatus' "ACCESS_DENIED"

pattern Detached :: ActiveDirectoryStatus
pattern Detached = ActiveDirectoryStatus' "DETACHED"

pattern Joined :: ActiveDirectoryStatus
pattern Joined = ActiveDirectoryStatus' "JOINED"

pattern Joining :: ActiveDirectoryStatus
pattern Joining = ActiveDirectoryStatus' "JOINING"

pattern NetworkError :: ActiveDirectoryStatus
pattern NetworkError = ActiveDirectoryStatus' "NETWORK_ERROR"

pattern Timeout :: ActiveDirectoryStatus
pattern Timeout = ActiveDirectoryStatus' "TIMEOUT"

pattern UnknownError :: ActiveDirectoryStatus
pattern UnknownError = ActiveDirectoryStatus' "UNKNOWN_ERROR"

{-# COMPLETE
  AccessDenied,
  Detached,
  Joined,
  Joining,
  NetworkError,
  Timeout,
  UnknownError,
  ActiveDirectoryStatus'
  #-}
