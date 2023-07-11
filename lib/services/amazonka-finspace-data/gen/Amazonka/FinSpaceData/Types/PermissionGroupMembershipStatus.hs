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
-- Module      : Amazonka.FinSpaceData.Types.PermissionGroupMembershipStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.PermissionGroupMembershipStatus
  ( PermissionGroupMembershipStatus
      ( ..,
        PermissionGroupMembershipStatus_ADDITION_IN_PROGRESS,
        PermissionGroupMembershipStatus_ADDITION_SUCCESS,
        PermissionGroupMembershipStatus_REMOVAL_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PermissionGroupMembershipStatus = PermissionGroupMembershipStatus'
  { fromPermissionGroupMembershipStatus ::
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

pattern PermissionGroupMembershipStatus_ADDITION_IN_PROGRESS :: PermissionGroupMembershipStatus
pattern PermissionGroupMembershipStatus_ADDITION_IN_PROGRESS = PermissionGroupMembershipStatus' "ADDITION_IN_PROGRESS"

pattern PermissionGroupMembershipStatus_ADDITION_SUCCESS :: PermissionGroupMembershipStatus
pattern PermissionGroupMembershipStatus_ADDITION_SUCCESS = PermissionGroupMembershipStatus' "ADDITION_SUCCESS"

pattern PermissionGroupMembershipStatus_REMOVAL_IN_PROGRESS :: PermissionGroupMembershipStatus
pattern PermissionGroupMembershipStatus_REMOVAL_IN_PROGRESS = PermissionGroupMembershipStatus' "REMOVAL_IN_PROGRESS"

{-# COMPLETE
  PermissionGroupMembershipStatus_ADDITION_IN_PROGRESS,
  PermissionGroupMembershipStatus_ADDITION_SUCCESS,
  PermissionGroupMembershipStatus_REMOVAL_IN_PROGRESS,
  PermissionGroupMembershipStatus'
  #-}
