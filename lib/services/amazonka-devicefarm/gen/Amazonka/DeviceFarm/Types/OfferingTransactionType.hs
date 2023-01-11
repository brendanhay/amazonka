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
-- Module      : Amazonka.DeviceFarm.Types.OfferingTransactionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.OfferingTransactionType
  ( OfferingTransactionType
      ( ..,
        OfferingTransactionType_PURCHASE,
        OfferingTransactionType_RENEW,
        OfferingTransactionType_SYSTEM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OfferingTransactionType = OfferingTransactionType'
  { fromOfferingTransactionType ::
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

pattern OfferingTransactionType_PURCHASE :: OfferingTransactionType
pattern OfferingTransactionType_PURCHASE = OfferingTransactionType' "PURCHASE"

pattern OfferingTransactionType_RENEW :: OfferingTransactionType
pattern OfferingTransactionType_RENEW = OfferingTransactionType' "RENEW"

pattern OfferingTransactionType_SYSTEM :: OfferingTransactionType
pattern OfferingTransactionType_SYSTEM = OfferingTransactionType' "SYSTEM"

{-# COMPLETE
  OfferingTransactionType_PURCHASE,
  OfferingTransactionType_RENEW,
  OfferingTransactionType_SYSTEM,
  OfferingTransactionType'
  #-}
