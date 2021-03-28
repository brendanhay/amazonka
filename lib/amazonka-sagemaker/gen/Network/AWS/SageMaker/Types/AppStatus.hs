{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppStatus
  ( AppStatus
    ( AppStatus'
    , AppStatusDeleted
    , AppStatusDeleting
    , AppStatusFailed
    , AppStatusInService
    , AppStatusPending
    , fromAppStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AppStatus = AppStatus'{fromAppStatus :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern AppStatusDeleted :: AppStatus
pattern AppStatusDeleted = AppStatus' "Deleted"

pattern AppStatusDeleting :: AppStatus
pattern AppStatusDeleting = AppStatus' "Deleting"

pattern AppStatusFailed :: AppStatus
pattern AppStatusFailed = AppStatus' "Failed"

pattern AppStatusInService :: AppStatus
pattern AppStatusInService = AppStatus' "InService"

pattern AppStatusPending :: AppStatus
pattern AppStatusPending = AppStatus' "Pending"

{-# COMPLETE 
  AppStatusDeleted,

  AppStatusDeleting,

  AppStatusFailed,

  AppStatusInService,

  AppStatusPending,
  AppStatus'
  #-}
