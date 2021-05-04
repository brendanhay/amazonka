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

import qualified Network.AWS.Prelude as Prelude

newtype AccountRoleStatus = AccountRoleStatus'
  { fromAccountRoleStatus ::
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
