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
-- Module      : Amazonka.Route53Domains.Types.StatusFlag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.StatusFlag
  ( StatusFlag
      ( ..,
        StatusFlag_PENDING_ACCEPTANCE,
        StatusFlag_PENDING_AUTHORIZATION,
        StatusFlag_PENDING_CUSTOMER_ACTION,
        StatusFlag_PENDING_PAYMENT_VERIFICATION,
        StatusFlag_PENDING_SUPPORT_CASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatusFlag = StatusFlag'
  { fromStatusFlag ::
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

pattern StatusFlag_PENDING_ACCEPTANCE :: StatusFlag
pattern StatusFlag_PENDING_ACCEPTANCE = StatusFlag' "PENDING_ACCEPTANCE"

pattern StatusFlag_PENDING_AUTHORIZATION :: StatusFlag
pattern StatusFlag_PENDING_AUTHORIZATION = StatusFlag' "PENDING_AUTHORIZATION"

pattern StatusFlag_PENDING_CUSTOMER_ACTION :: StatusFlag
pattern StatusFlag_PENDING_CUSTOMER_ACTION = StatusFlag' "PENDING_CUSTOMER_ACTION"

pattern StatusFlag_PENDING_PAYMENT_VERIFICATION :: StatusFlag
pattern StatusFlag_PENDING_PAYMENT_VERIFICATION = StatusFlag' "PENDING_PAYMENT_VERIFICATION"

pattern StatusFlag_PENDING_SUPPORT_CASE :: StatusFlag
pattern StatusFlag_PENDING_SUPPORT_CASE = StatusFlag' "PENDING_SUPPORT_CASE"

{-# COMPLETE
  StatusFlag_PENDING_ACCEPTANCE,
  StatusFlag_PENDING_AUTHORIZATION,
  StatusFlag_PENDING_CUSTOMER_ACTION,
  StatusFlag_PENDING_PAYMENT_VERIFICATION,
  StatusFlag_PENDING_SUPPORT_CASE,
  StatusFlag'
  #-}
