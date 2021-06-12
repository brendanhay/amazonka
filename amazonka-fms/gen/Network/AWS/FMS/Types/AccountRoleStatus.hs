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
-- Module      : Network.AWS.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AccountRoleStatus
  ( AccountRoleStatus
      ( ..,
        AccountRoleStatus_CREATING,
        AccountRoleStatus_DELETED,
        AccountRoleStatus_DELETING,
        AccountRoleStatus_PENDING_DELETION,
        AccountRoleStatus_READY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AccountRoleStatus = AccountRoleStatus'
  { fromAccountRoleStatus ::
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

pattern AccountRoleStatus_CREATING :: AccountRoleStatus
pattern AccountRoleStatus_CREATING = AccountRoleStatus' "CREATING"

pattern AccountRoleStatus_DELETED :: AccountRoleStatus
pattern AccountRoleStatus_DELETED = AccountRoleStatus' "DELETED"

pattern AccountRoleStatus_DELETING :: AccountRoleStatus
pattern AccountRoleStatus_DELETING = AccountRoleStatus' "DELETING"

pattern AccountRoleStatus_PENDING_DELETION :: AccountRoleStatus
pattern AccountRoleStatus_PENDING_DELETION = AccountRoleStatus' "PENDING_DELETION"

pattern AccountRoleStatus_READY :: AccountRoleStatus
pattern AccountRoleStatus_READY = AccountRoleStatus' "READY"

{-# COMPLETE
  AccountRoleStatus_CREATING,
  AccountRoleStatus_DELETED,
  AccountRoleStatus_DELETING,
  AccountRoleStatus_PENDING_DELETION,
  AccountRoleStatus_READY,
  AccountRoleStatus'
  #-}
