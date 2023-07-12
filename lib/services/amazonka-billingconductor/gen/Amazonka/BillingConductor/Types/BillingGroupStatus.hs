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
-- Module      : Amazonka.BillingConductor.Types.BillingGroupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.BillingGroupStatus
  ( BillingGroupStatus
      ( ..,
        BillingGroupStatus_ACTIVE,
        BillingGroupStatus_PRIMARY_ACCOUNT_MISSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BillingGroupStatus = BillingGroupStatus'
  { fromBillingGroupStatus ::
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

pattern BillingGroupStatus_ACTIVE :: BillingGroupStatus
pattern BillingGroupStatus_ACTIVE = BillingGroupStatus' "ACTIVE"

pattern BillingGroupStatus_PRIMARY_ACCOUNT_MISSING :: BillingGroupStatus
pattern BillingGroupStatus_PRIMARY_ACCOUNT_MISSING = BillingGroupStatus' "PRIMARY_ACCOUNT_MISSING"

{-# COMPLETE
  BillingGroupStatus_ACTIVE,
  BillingGroupStatus_PRIMARY_ACCOUNT_MISSING,
  BillingGroupStatus'
  #-}
