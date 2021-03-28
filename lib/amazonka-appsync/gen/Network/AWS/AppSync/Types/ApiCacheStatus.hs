{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ApiCacheStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ApiCacheStatus
  ( ApiCacheStatus
    ( ApiCacheStatus'
    , ApiCacheStatusAvailable
    , ApiCacheStatusCreating
    , ApiCacheStatusDeleting
    , ApiCacheStatusModifying
    , ApiCacheStatusFailed
    , fromApiCacheStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ApiCacheStatus = ApiCacheStatus'{fromApiCacheStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern ApiCacheStatusAvailable :: ApiCacheStatus
pattern ApiCacheStatusAvailable = ApiCacheStatus' "AVAILABLE"

pattern ApiCacheStatusCreating :: ApiCacheStatus
pattern ApiCacheStatusCreating = ApiCacheStatus' "CREATING"

pattern ApiCacheStatusDeleting :: ApiCacheStatus
pattern ApiCacheStatusDeleting = ApiCacheStatus' "DELETING"

pattern ApiCacheStatusModifying :: ApiCacheStatus
pattern ApiCacheStatusModifying = ApiCacheStatus' "MODIFYING"

pattern ApiCacheStatusFailed :: ApiCacheStatus
pattern ApiCacheStatusFailed = ApiCacheStatus' "FAILED"

{-# COMPLETE 
  ApiCacheStatusAvailable,

  ApiCacheStatusCreating,

  ApiCacheStatusDeleting,

  ApiCacheStatusModifying,

  ApiCacheStatusFailed,
  ApiCacheStatus'
  #-}
