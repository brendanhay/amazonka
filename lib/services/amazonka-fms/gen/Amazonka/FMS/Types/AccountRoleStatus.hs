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
-- Module      : Amazonka.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AccountRoleStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountRoleStatus = AccountRoleStatus'
  { fromAccountRoleStatus ::
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
