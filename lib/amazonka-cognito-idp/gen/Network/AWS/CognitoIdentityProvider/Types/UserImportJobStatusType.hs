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
        Created,
        Expired,
        Failed,
        InProgress,
        Pending,
        Stopped,
        Stopping,
        Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserImportJobStatusType = UserImportJobStatusType' Lude.Text
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

pattern Created :: UserImportJobStatusType
pattern Created = UserImportJobStatusType' "Created"

pattern Expired :: UserImportJobStatusType
pattern Expired = UserImportJobStatusType' "Expired"

pattern Failed :: UserImportJobStatusType
pattern Failed = UserImportJobStatusType' "Failed"

pattern InProgress :: UserImportJobStatusType
pattern InProgress = UserImportJobStatusType' "InProgress"

pattern Pending :: UserImportJobStatusType
pattern Pending = UserImportJobStatusType' "Pending"

pattern Stopped :: UserImportJobStatusType
pattern Stopped = UserImportJobStatusType' "Stopped"

pattern Stopping :: UserImportJobStatusType
pattern Stopping = UserImportJobStatusType' "Stopping"

pattern Succeeded :: UserImportJobStatusType
pattern Succeeded = UserImportJobStatusType' "Succeeded"

{-# COMPLETE
  Created,
  Expired,
  Failed,
  InProgress,
  Pending,
  Stopped,
  Stopping,
  Succeeded,
  UserImportJobStatusType'
  #-}
