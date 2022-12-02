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
-- Module      : Amazonka.Outposts.Types.PaymentOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.PaymentOption
  ( PaymentOption
      ( ..,
        PaymentOption_ALL_UPFRONT,
        PaymentOption_NO_UPFRONT,
        PaymentOption_PARTIAL_UPFRONT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PaymentOption = PaymentOption'
  { fromPaymentOption ::
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

pattern PaymentOption_ALL_UPFRONT :: PaymentOption
pattern PaymentOption_ALL_UPFRONT = PaymentOption' "ALL_UPFRONT"

pattern PaymentOption_NO_UPFRONT :: PaymentOption
pattern PaymentOption_NO_UPFRONT = PaymentOption' "NO_UPFRONT"

pattern PaymentOption_PARTIAL_UPFRONT :: PaymentOption
pattern PaymentOption_PARTIAL_UPFRONT = PaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  PaymentOption_ALL_UPFRONT,
  PaymentOption_NO_UPFRONT,
  PaymentOption_PARTIAL_UPFRONT,
  PaymentOption'
  #-}
