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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansDataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansDataType
  ( SavingsPlansDataType
      ( ..,
        SavingsPlansDataType_AMORTIZED_COMMITMENT,
        SavingsPlansDataType_ATTRIBUTES,
        SavingsPlansDataType_SAVINGS,
        SavingsPlansDataType_UTILIZATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlansDataType = SavingsPlansDataType'
  { fromSavingsPlansDataType ::
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

pattern SavingsPlansDataType_AMORTIZED_COMMITMENT :: SavingsPlansDataType
pattern SavingsPlansDataType_AMORTIZED_COMMITMENT = SavingsPlansDataType' "AMORTIZED_COMMITMENT"

pattern SavingsPlansDataType_ATTRIBUTES :: SavingsPlansDataType
pattern SavingsPlansDataType_ATTRIBUTES = SavingsPlansDataType' "ATTRIBUTES"

pattern SavingsPlansDataType_SAVINGS :: SavingsPlansDataType
pattern SavingsPlansDataType_SAVINGS = SavingsPlansDataType' "SAVINGS"

pattern SavingsPlansDataType_UTILIZATION :: SavingsPlansDataType
pattern SavingsPlansDataType_UTILIZATION = SavingsPlansDataType' "UTILIZATION"

{-# COMPLETE
  SavingsPlansDataType_AMORTIZED_COMMITMENT,
  SavingsPlansDataType_ATTRIBUTES,
  SavingsPlansDataType_SAVINGS,
  SavingsPlansDataType_UTILIZATION,
  SavingsPlansDataType'
  #-}
