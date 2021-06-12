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
-- Module      : Network.AWS.Config.Types.MemberAccountRuleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountRuleStatus
  ( MemberAccountRuleStatus
      ( ..,
        MemberAccountRuleStatus_CREATE_FAILED,
        MemberAccountRuleStatus_CREATE_IN_PROGRESS,
        MemberAccountRuleStatus_CREATE_SUCCESSFUL,
        MemberAccountRuleStatus_DELETE_FAILED,
        MemberAccountRuleStatus_DELETE_IN_PROGRESS,
        MemberAccountRuleStatus_DELETE_SUCCESSFUL,
        MemberAccountRuleStatus_UPDATE_FAILED,
        MemberAccountRuleStatus_UPDATE_IN_PROGRESS,
        MemberAccountRuleStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MemberAccountRuleStatus = MemberAccountRuleStatus'
  { fromMemberAccountRuleStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MemberAccountRuleStatus_CREATE_FAILED :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_CREATE_FAILED = MemberAccountRuleStatus' "CREATE_FAILED"

pattern MemberAccountRuleStatus_CREATE_IN_PROGRESS :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_CREATE_IN_PROGRESS = MemberAccountRuleStatus' "CREATE_IN_PROGRESS"

pattern MemberAccountRuleStatus_CREATE_SUCCESSFUL :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_CREATE_SUCCESSFUL = MemberAccountRuleStatus' "CREATE_SUCCESSFUL"

pattern MemberAccountRuleStatus_DELETE_FAILED :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_DELETE_FAILED = MemberAccountRuleStatus' "DELETE_FAILED"

pattern MemberAccountRuleStatus_DELETE_IN_PROGRESS :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_DELETE_IN_PROGRESS = MemberAccountRuleStatus' "DELETE_IN_PROGRESS"

pattern MemberAccountRuleStatus_DELETE_SUCCESSFUL :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_DELETE_SUCCESSFUL = MemberAccountRuleStatus' "DELETE_SUCCESSFUL"

pattern MemberAccountRuleStatus_UPDATE_FAILED :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_UPDATE_FAILED = MemberAccountRuleStatus' "UPDATE_FAILED"

pattern MemberAccountRuleStatus_UPDATE_IN_PROGRESS :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_UPDATE_IN_PROGRESS = MemberAccountRuleStatus' "UPDATE_IN_PROGRESS"

pattern MemberAccountRuleStatus_UPDATE_SUCCESSFUL :: MemberAccountRuleStatus
pattern MemberAccountRuleStatus_UPDATE_SUCCESSFUL = MemberAccountRuleStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  MemberAccountRuleStatus_CREATE_FAILED,
  MemberAccountRuleStatus_CREATE_IN_PROGRESS,
  MemberAccountRuleStatus_CREATE_SUCCESSFUL,
  MemberAccountRuleStatus_DELETE_FAILED,
  MemberAccountRuleStatus_DELETE_IN_PROGRESS,
  MemberAccountRuleStatus_DELETE_SUCCESSFUL,
  MemberAccountRuleStatus_UPDATE_FAILED,
  MemberAccountRuleStatus_UPDATE_IN_PROGRESS,
  MemberAccountRuleStatus_UPDATE_SUCCESSFUL,
  MemberAccountRuleStatus'
  #-}
