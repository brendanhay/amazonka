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
-- Module      : Amazonka.AuditManager.Types.AccountStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AccountStatus
  ( AccountStatus
      ( ..,
        AccountStatus_ACTIVE,
        AccountStatus_INACTIVE,
        AccountStatus_PENDING_ACTIVATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountStatus = AccountStatus'
  { fromAccountStatus ::
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

pattern AccountStatus_ACTIVE :: AccountStatus
pattern AccountStatus_ACTIVE = AccountStatus' "ACTIVE"

pattern AccountStatus_INACTIVE :: AccountStatus
pattern AccountStatus_INACTIVE = AccountStatus' "INACTIVE"

pattern AccountStatus_PENDING_ACTIVATION :: AccountStatus
pattern AccountStatus_PENDING_ACTIVATION = AccountStatus' "PENDING_ACTIVATION"

{-# COMPLETE
  AccountStatus_ACTIVE,
  AccountStatus_INACTIVE,
  AccountStatus_PENDING_ACTIVATION,
  AccountStatus'
  #-}
