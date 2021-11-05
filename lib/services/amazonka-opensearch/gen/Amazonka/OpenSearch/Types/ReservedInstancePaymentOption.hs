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
-- Module      : Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
  ( ReservedInstancePaymentOption
      ( ..,
        ReservedInstancePaymentOption_ALL_UPFRONT,
        ReservedInstancePaymentOption_NO_UPFRONT,
        ReservedInstancePaymentOption_PARTIAL_UPFRONT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReservedInstancePaymentOption = ReservedInstancePaymentOption'
  { fromReservedInstancePaymentOption ::
      Core.Text
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

pattern ReservedInstancePaymentOption_ALL_UPFRONT :: ReservedInstancePaymentOption
pattern ReservedInstancePaymentOption_ALL_UPFRONT = ReservedInstancePaymentOption' "ALL_UPFRONT"

pattern ReservedInstancePaymentOption_NO_UPFRONT :: ReservedInstancePaymentOption
pattern ReservedInstancePaymentOption_NO_UPFRONT = ReservedInstancePaymentOption' "NO_UPFRONT"

pattern ReservedInstancePaymentOption_PARTIAL_UPFRONT :: ReservedInstancePaymentOption
pattern ReservedInstancePaymentOption_PARTIAL_UPFRONT = ReservedInstancePaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  ReservedInstancePaymentOption_ALL_UPFRONT,
  ReservedInstancePaymentOption_NO_UPFRONT,
  ReservedInstancePaymentOption_PARTIAL_UPFRONT,
  ReservedInstancePaymentOption'
  #-}
