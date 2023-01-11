{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.UserProfileStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserProfileStatus
  ( UserProfileStatus
      ( ..,
        UserProfileStatus_Delete_Failed,
        UserProfileStatus_Deleting,
        UserProfileStatus_Failed,
        UserProfileStatus_InService,
        UserProfileStatus_Pending,
        UserProfileStatus_Update_Failed,
        UserProfileStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UserProfileStatus = UserProfileStatus'
  { fromUserProfileStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern UserProfileStatus_Delete_Failed :: UserProfileStatus
pattern UserProfileStatus_Delete_Failed = UserProfileStatus' "Delete_Failed"

pattern UserProfileStatus_Deleting :: UserProfileStatus
pattern UserProfileStatus_Deleting = UserProfileStatus' "Deleting"

pattern UserProfileStatus_Failed :: UserProfileStatus
pattern UserProfileStatus_Failed = UserProfileStatus' "Failed"

pattern UserProfileStatus_InService :: UserProfileStatus
pattern UserProfileStatus_InService = UserProfileStatus' "InService"

pattern UserProfileStatus_Pending :: UserProfileStatus
pattern UserProfileStatus_Pending = UserProfileStatus' "Pending"

pattern UserProfileStatus_Update_Failed :: UserProfileStatus
pattern UserProfileStatus_Update_Failed = UserProfileStatus' "Update_Failed"

pattern UserProfileStatus_Updating :: UserProfileStatus
pattern UserProfileStatus_Updating = UserProfileStatus' "Updating"

{-# COMPLETE
  UserProfileStatus_Delete_Failed,
  UserProfileStatus_Deleting,
  UserProfileStatus_Failed,
  UserProfileStatus_InService,
  UserProfileStatus_Pending,
  UserProfileStatus_Update_Failed,
  UserProfileStatus_Updating,
  UserProfileStatus'
  #-}
