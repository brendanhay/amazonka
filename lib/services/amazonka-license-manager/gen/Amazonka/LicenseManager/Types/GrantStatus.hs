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
-- Module      : Amazonka.LicenseManager.Types.GrantStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.GrantStatus
  ( GrantStatus
      ( ..,
        GrantStatus_ACTIVE,
        GrantStatus_DELETED,
        GrantStatus_DISABLED,
        GrantStatus_FAILED_WORKFLOW,
        GrantStatus_PENDING_ACCEPT,
        GrantStatus_PENDING_DELETE,
        GrantStatus_PENDING_WORKFLOW,
        GrantStatus_REJECTED,
        GrantStatus_WORKFLOW_COMPLETED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GrantStatus = GrantStatus'
  { fromGrantStatus ::
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

pattern GrantStatus_ACTIVE :: GrantStatus
pattern GrantStatus_ACTIVE = GrantStatus' "ACTIVE"

pattern GrantStatus_DELETED :: GrantStatus
pattern GrantStatus_DELETED = GrantStatus' "DELETED"

pattern GrantStatus_DISABLED :: GrantStatus
pattern GrantStatus_DISABLED = GrantStatus' "DISABLED"

pattern GrantStatus_FAILED_WORKFLOW :: GrantStatus
pattern GrantStatus_FAILED_WORKFLOW = GrantStatus' "FAILED_WORKFLOW"

pattern GrantStatus_PENDING_ACCEPT :: GrantStatus
pattern GrantStatus_PENDING_ACCEPT = GrantStatus' "PENDING_ACCEPT"

pattern GrantStatus_PENDING_DELETE :: GrantStatus
pattern GrantStatus_PENDING_DELETE = GrantStatus' "PENDING_DELETE"

pattern GrantStatus_PENDING_WORKFLOW :: GrantStatus
pattern GrantStatus_PENDING_WORKFLOW = GrantStatus' "PENDING_WORKFLOW"

pattern GrantStatus_REJECTED :: GrantStatus
pattern GrantStatus_REJECTED = GrantStatus' "REJECTED"

pattern GrantStatus_WORKFLOW_COMPLETED :: GrantStatus
pattern GrantStatus_WORKFLOW_COMPLETED = GrantStatus' "WORKFLOW_COMPLETED"

{-# COMPLETE
  GrantStatus_ACTIVE,
  GrantStatus_DELETED,
  GrantStatus_DISABLED,
  GrantStatus_FAILED_WORKFLOW,
  GrantStatus_PENDING_ACCEPT,
  GrantStatus_PENDING_DELETE,
  GrantStatus_PENDING_WORKFLOW,
  GrantStatus_REJECTED,
  GrantStatus_WORKFLOW_COMPLETED,
  GrantStatus'
  #-}
