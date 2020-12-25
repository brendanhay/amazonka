{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileStatus
  ( UserProfileStatus
      ( UserProfileStatus',
        UserProfileStatusDeleting,
        UserProfileStatusFailed,
        UserProfileStatusInService,
        UserProfileStatusPending,
        UserProfileStatusUpdating,
        UserProfileStatusUpdateFailed,
        UserProfileStatusDeleteFailed,
        fromUserProfileStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UserProfileStatus = UserProfileStatus'
  { fromUserProfileStatus ::
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

pattern UserProfileStatusDeleting :: UserProfileStatus
pattern UserProfileStatusDeleting = UserProfileStatus' "Deleting"

pattern UserProfileStatusFailed :: UserProfileStatus
pattern UserProfileStatusFailed = UserProfileStatus' "Failed"

pattern UserProfileStatusInService :: UserProfileStatus
pattern UserProfileStatusInService = UserProfileStatus' "InService"

pattern UserProfileStatusPending :: UserProfileStatus
pattern UserProfileStatusPending = UserProfileStatus' "Pending"

pattern UserProfileStatusUpdating :: UserProfileStatus
pattern UserProfileStatusUpdating = UserProfileStatus' "Updating"

pattern UserProfileStatusUpdateFailed :: UserProfileStatus
pattern UserProfileStatusUpdateFailed = UserProfileStatus' "Update_Failed"

pattern UserProfileStatusDeleteFailed :: UserProfileStatus
pattern UserProfileStatusDeleteFailed = UserProfileStatus' "Delete_Failed"

{-# COMPLETE
  UserProfileStatusDeleting,
  UserProfileStatusFailed,
  UserProfileStatusInService,
  UserProfileStatusPending,
  UserProfileStatusUpdating,
  UserProfileStatusUpdateFailed,
  UserProfileStatusDeleteFailed,
  UserProfileStatus'
  #-}
