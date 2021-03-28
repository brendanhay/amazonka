{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnRouteStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnRouteStatusCode
  ( ClientVpnRouteStatusCode
    ( ClientVpnRouteStatusCode'
    , ClientVpnRouteStatusCodeCreating
    , ClientVpnRouteStatusCodeActive
    , ClientVpnRouteStatusCodeFailed
    , ClientVpnRouteStatusCodeDeleting
    , fromClientVpnRouteStatusCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ClientVpnRouteStatusCode = ClientVpnRouteStatusCode'{fromClientVpnRouteStatusCode
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern ClientVpnRouteStatusCodeCreating :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCodeCreating = ClientVpnRouteStatusCode' "creating"

pattern ClientVpnRouteStatusCodeActive :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCodeActive = ClientVpnRouteStatusCode' "active"

pattern ClientVpnRouteStatusCodeFailed :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCodeFailed = ClientVpnRouteStatusCode' "failed"

pattern ClientVpnRouteStatusCodeDeleting :: ClientVpnRouteStatusCode
pattern ClientVpnRouteStatusCodeDeleting = ClientVpnRouteStatusCode' "deleting"

{-# COMPLETE 
  ClientVpnRouteStatusCodeCreating,

  ClientVpnRouteStatusCodeActive,

  ClientVpnRouteStatusCodeFailed,

  ClientVpnRouteStatusCodeDeleting,
  ClientVpnRouteStatusCode'
  #-}
