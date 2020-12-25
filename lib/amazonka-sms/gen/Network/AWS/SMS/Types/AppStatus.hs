{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppStatus
  ( AppStatus
      ( AppStatus',
        AppStatusCreating,
        AppStatusActive,
        AppStatusUpdating,
        AppStatusDeleting,
        AppStatusDeleted,
        AppStatusDeleteFailed,
        fromAppStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AppStatus = AppStatus' {fromAppStatus :: Core.Text}
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

pattern AppStatusCreating :: AppStatus
pattern AppStatusCreating = AppStatus' "CREATING"

pattern AppStatusActive :: AppStatus
pattern AppStatusActive = AppStatus' "ACTIVE"

pattern AppStatusUpdating :: AppStatus
pattern AppStatusUpdating = AppStatus' "UPDATING"

pattern AppStatusDeleting :: AppStatus
pattern AppStatusDeleting = AppStatus' "DELETING"

pattern AppStatusDeleted :: AppStatus
pattern AppStatusDeleted = AppStatus' "DELETED"

pattern AppStatusDeleteFailed :: AppStatus
pattern AppStatusDeleteFailed = AppStatus' "DELETE_FAILED"

{-# COMPLETE
  AppStatusCreating,
  AppStatusActive,
  AppStatusUpdating,
  AppStatusDeleting,
  AppStatusDeleted,
  AppStatusDeleteFailed,
  AppStatus'
  #-}
