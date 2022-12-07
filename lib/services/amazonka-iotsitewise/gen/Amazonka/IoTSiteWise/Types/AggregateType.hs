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
-- Module      : Amazonka.IoTSiteWise.Types.AggregateType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AggregateType
  ( AggregateType
      ( ..,
        AggregateType_AVERAGE,
        AggregateType_COUNT,
        AggregateType_MAXIMUM,
        AggregateType_MINIMUM,
        AggregateType_STANDARD_DEVIATION,
        AggregateType_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregateType = AggregateType'
  { fromAggregateType ::
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

pattern AggregateType_AVERAGE :: AggregateType
pattern AggregateType_AVERAGE = AggregateType' "AVERAGE"

pattern AggregateType_COUNT :: AggregateType
pattern AggregateType_COUNT = AggregateType' "COUNT"

pattern AggregateType_MAXIMUM :: AggregateType
pattern AggregateType_MAXIMUM = AggregateType' "MAXIMUM"

pattern AggregateType_MINIMUM :: AggregateType
pattern AggregateType_MINIMUM = AggregateType' "MINIMUM"

pattern AggregateType_STANDARD_DEVIATION :: AggregateType
pattern AggregateType_STANDARD_DEVIATION = AggregateType' "STANDARD_DEVIATION"

pattern AggregateType_SUM :: AggregateType
pattern AggregateType_SUM = AggregateType' "SUM"

{-# COMPLETE
  AggregateType_AVERAGE,
  AggregateType_COUNT,
  AggregateType_MAXIMUM,
  AggregateType_MINIMUM,
  AggregateType_STANDARD_DEVIATION,
  AggregateType_SUM,
  AggregateType'
  #-}
