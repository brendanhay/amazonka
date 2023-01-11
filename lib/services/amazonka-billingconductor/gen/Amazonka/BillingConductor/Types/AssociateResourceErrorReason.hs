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
-- Module      : Amazonka.BillingConductor.Types.AssociateResourceErrorReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.AssociateResourceErrorReason
  ( AssociateResourceErrorReason
      ( ..,
        AssociateResourceErrorReason_ILLEGAL_CUSTOMLINEITEM,
        AssociateResourceErrorReason_INTERNAL_SERVER_EXCEPTION,
        AssociateResourceErrorReason_INVALID_ARN,
        AssociateResourceErrorReason_INVALID_BILLING_PERIOD_RANGE,
        AssociateResourceErrorReason_SERVICE_LIMIT_EXCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociateResourceErrorReason = AssociateResourceErrorReason'
  { fromAssociateResourceErrorReason ::
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

pattern AssociateResourceErrorReason_ILLEGAL_CUSTOMLINEITEM :: AssociateResourceErrorReason
pattern AssociateResourceErrorReason_ILLEGAL_CUSTOMLINEITEM = AssociateResourceErrorReason' "ILLEGAL_CUSTOMLINEITEM"

pattern AssociateResourceErrorReason_INTERNAL_SERVER_EXCEPTION :: AssociateResourceErrorReason
pattern AssociateResourceErrorReason_INTERNAL_SERVER_EXCEPTION = AssociateResourceErrorReason' "INTERNAL_SERVER_EXCEPTION"

pattern AssociateResourceErrorReason_INVALID_ARN :: AssociateResourceErrorReason
pattern AssociateResourceErrorReason_INVALID_ARN = AssociateResourceErrorReason' "INVALID_ARN"

pattern AssociateResourceErrorReason_INVALID_BILLING_PERIOD_RANGE :: AssociateResourceErrorReason
pattern AssociateResourceErrorReason_INVALID_BILLING_PERIOD_RANGE = AssociateResourceErrorReason' "INVALID_BILLING_PERIOD_RANGE"

pattern AssociateResourceErrorReason_SERVICE_LIMIT_EXCEEDED :: AssociateResourceErrorReason
pattern AssociateResourceErrorReason_SERVICE_LIMIT_EXCEEDED = AssociateResourceErrorReason' "SERVICE_LIMIT_EXCEEDED"

{-# COMPLETE
  AssociateResourceErrorReason_ILLEGAL_CUSTOMLINEITEM,
  AssociateResourceErrorReason_INTERNAL_SERVER_EXCEPTION,
  AssociateResourceErrorReason_INVALID_ARN,
  AssociateResourceErrorReason_INVALID_BILLING_PERIOD_RANGE,
  AssociateResourceErrorReason_SERVICE_LIMIT_EXCEEDED,
  AssociateResourceErrorReason'
  #-}
