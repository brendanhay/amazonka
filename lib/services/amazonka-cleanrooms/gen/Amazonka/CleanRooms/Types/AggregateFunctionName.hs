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
-- Module      : Amazonka.CleanRooms.Types.AggregateFunctionName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AggregateFunctionName
  ( AggregateFunctionName
      ( ..,
        AggregateFunctionName_AVG,
        AggregateFunctionName_COUNT,
        AggregateFunctionName_COUNT_DISTINCT,
        AggregateFunctionName_SUM,
        AggregateFunctionName_SUM_DISTINCT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregateFunctionName = AggregateFunctionName'
  { fromAggregateFunctionName ::
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

pattern AggregateFunctionName_AVG :: AggregateFunctionName
pattern AggregateFunctionName_AVG = AggregateFunctionName' "AVG"

pattern AggregateFunctionName_COUNT :: AggregateFunctionName
pattern AggregateFunctionName_COUNT = AggregateFunctionName' "COUNT"

pattern AggregateFunctionName_COUNT_DISTINCT :: AggregateFunctionName
pattern AggregateFunctionName_COUNT_DISTINCT = AggregateFunctionName' "COUNT_DISTINCT"

pattern AggregateFunctionName_SUM :: AggregateFunctionName
pattern AggregateFunctionName_SUM = AggregateFunctionName' "SUM"

pattern AggregateFunctionName_SUM_DISTINCT :: AggregateFunctionName
pattern AggregateFunctionName_SUM_DISTINCT = AggregateFunctionName' "SUM_DISTINCT"

{-# COMPLETE
  AggregateFunctionName_AVG,
  AggregateFunctionName_COUNT,
  AggregateFunctionName_COUNT_DISTINCT,
  AggregateFunctionName_SUM,
  AggregateFunctionName_SUM_DISTINCT,
  AggregateFunctionName'
  #-}
