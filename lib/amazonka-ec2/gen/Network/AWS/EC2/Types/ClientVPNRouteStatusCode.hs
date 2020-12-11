-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRouteStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRouteStatusCode
  ( ClientVPNRouteStatusCode
      ( ClientVPNRouteStatusCode',
        CVRSCActive,
        CVRSCCreating,
        CVRSCDeleting,
        CVRSCFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ClientVPNRouteStatusCode = ClientVPNRouteStatusCode' Lude.Text
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

pattern CVRSCActive :: ClientVPNRouteStatusCode
pattern CVRSCActive = ClientVPNRouteStatusCode' "active"

pattern CVRSCCreating :: ClientVPNRouteStatusCode
pattern CVRSCCreating = ClientVPNRouteStatusCode' "creating"

pattern CVRSCDeleting :: ClientVPNRouteStatusCode
pattern CVRSCDeleting = ClientVPNRouteStatusCode' "deleting"

pattern CVRSCFailed :: ClientVPNRouteStatusCode
pattern CVRSCFailed = ClientVPNRouteStatusCode' "failed"

{-# COMPLETE
  CVRSCActive,
  CVRSCCreating,
  CVRSCDeleting,
  CVRSCFailed,
  ClientVPNRouteStatusCode'
  #-}
