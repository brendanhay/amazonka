{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
  ( UserImportJobStatusType
      ( UserImportJobStatusType',
        UserImportJobStatusTypeCreated,
        UserImportJobStatusTypePending,
        UserImportJobStatusTypeInProgress,
        UserImportJobStatusTypeStopping,
        UserImportJobStatusTypeExpired,
        UserImportJobStatusTypeStopped,
        UserImportJobStatusTypeFailed,
        UserImportJobStatusTypeSucceeded,
        fromUserImportJobStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UserImportJobStatusType = UserImportJobStatusType'
  { fromUserImportJobStatusType ::
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

pattern UserImportJobStatusTypeCreated :: UserImportJobStatusType
pattern UserImportJobStatusTypeCreated = UserImportJobStatusType' "Created"

pattern UserImportJobStatusTypePending :: UserImportJobStatusType
pattern UserImportJobStatusTypePending = UserImportJobStatusType' "Pending"

pattern UserImportJobStatusTypeInProgress :: UserImportJobStatusType
pattern UserImportJobStatusTypeInProgress = UserImportJobStatusType' "InProgress"

pattern UserImportJobStatusTypeStopping :: UserImportJobStatusType
pattern UserImportJobStatusTypeStopping = UserImportJobStatusType' "Stopping"

pattern UserImportJobStatusTypeExpired :: UserImportJobStatusType
pattern UserImportJobStatusTypeExpired = UserImportJobStatusType' "Expired"

pattern UserImportJobStatusTypeStopped :: UserImportJobStatusType
pattern UserImportJobStatusTypeStopped = UserImportJobStatusType' "Stopped"

pattern UserImportJobStatusTypeFailed :: UserImportJobStatusType
pattern UserImportJobStatusTypeFailed = UserImportJobStatusType' "Failed"

pattern UserImportJobStatusTypeSucceeded :: UserImportJobStatusType
pattern UserImportJobStatusTypeSucceeded = UserImportJobStatusType' "Succeeded"

{-# COMPLETE
  UserImportJobStatusTypeCreated,
  UserImportJobStatusTypePending,
  UserImportJobStatusTypeInProgress,
  UserImportJobStatusTypeStopping,
  UserImportJobStatusTypeExpired,
  UserImportJobStatusTypeStopped,
  UserImportJobStatusTypeFailed,
  UserImportJobStatusTypeSucceeded,
  UserImportJobStatusType'
  #-}
