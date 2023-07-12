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
-- Module      : Amazonka.IoTEventsData.Types.ComparisonOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_EQUAL,
        ComparisonOperator_GREATER,
        ComparisonOperator_GREATER_OR_EQUAL,
        ComparisonOperator_LESS,
        ComparisonOperator_LESS_OR_EQUAL,
        ComparisonOperator_NOT_EQUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
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

pattern ComparisonOperator_EQUAL :: ComparisonOperator
pattern ComparisonOperator_EQUAL = ComparisonOperator' "EQUAL"

pattern ComparisonOperator_GREATER :: ComparisonOperator
pattern ComparisonOperator_GREATER = ComparisonOperator' "GREATER"

pattern ComparisonOperator_GREATER_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_GREATER_OR_EQUAL = ComparisonOperator' "GREATER_OR_EQUAL"

pattern ComparisonOperator_LESS :: ComparisonOperator
pattern ComparisonOperator_LESS = ComparisonOperator' "LESS"

pattern ComparisonOperator_LESS_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_LESS_OR_EQUAL = ComparisonOperator' "LESS_OR_EQUAL"

pattern ComparisonOperator_NOT_EQUAL :: ComparisonOperator
pattern ComparisonOperator_NOT_EQUAL = ComparisonOperator' "NOT_EQUAL"

{-# COMPLETE
  ComparisonOperator_EQUAL,
  ComparisonOperator_GREATER,
  ComparisonOperator_GREATER_OR_EQUAL,
  ComparisonOperator_LESS,
  ComparisonOperator_LESS_OR_EQUAL,
  ComparisonOperator_NOT_EQUAL,
  ComparisonOperator'
  #-}
