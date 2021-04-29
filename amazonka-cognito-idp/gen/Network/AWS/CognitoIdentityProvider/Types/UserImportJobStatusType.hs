{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
  ( UserImportJobStatusType
      ( ..,
        UserImportJobStatusType_Created,
        UserImportJobStatusType_Expired,
        UserImportJobStatusType_Failed,
        UserImportJobStatusType_InProgress,
        UserImportJobStatusType_Pending,
        UserImportJobStatusType_Stopped,
        UserImportJobStatusType_Stopping,
        UserImportJobStatusType_Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype UserImportJobStatusType = UserImportJobStatusType'
  { fromUserImportJobStatusType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern UserImportJobStatusType_Created :: UserImportJobStatusType
pattern UserImportJobStatusType_Created = UserImportJobStatusType' "Created"

pattern UserImportJobStatusType_Expired :: UserImportJobStatusType
pattern UserImportJobStatusType_Expired = UserImportJobStatusType' "Expired"

pattern UserImportJobStatusType_Failed :: UserImportJobStatusType
pattern UserImportJobStatusType_Failed = UserImportJobStatusType' "Failed"

pattern UserImportJobStatusType_InProgress :: UserImportJobStatusType
pattern UserImportJobStatusType_InProgress = UserImportJobStatusType' "InProgress"

pattern UserImportJobStatusType_Pending :: UserImportJobStatusType
pattern UserImportJobStatusType_Pending = UserImportJobStatusType' "Pending"

pattern UserImportJobStatusType_Stopped :: UserImportJobStatusType
pattern UserImportJobStatusType_Stopped = UserImportJobStatusType' "Stopped"

pattern UserImportJobStatusType_Stopping :: UserImportJobStatusType
pattern UserImportJobStatusType_Stopping = UserImportJobStatusType' "Stopping"

pattern UserImportJobStatusType_Succeeded :: UserImportJobStatusType
pattern UserImportJobStatusType_Succeeded = UserImportJobStatusType' "Succeeded"

{-# COMPLETE
  UserImportJobStatusType_Created,
  UserImportJobStatusType_Expired,
  UserImportJobStatusType_Failed,
  UserImportJobStatusType_InProgress,
  UserImportJobStatusType_Pending,
  UserImportJobStatusType_Stopped,
  UserImportJobStatusType_Stopping,
  UserImportJobStatusType_Succeeded,
  UserImportJobStatusType'
  #-}
