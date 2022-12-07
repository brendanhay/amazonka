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
-- Module      : Amazonka.LakeFormation.Types.ComparisonOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_BEGINS_WITH,
        ComparisonOperator_BETWEEN,
        ComparisonOperator_CONTAINS,
        ComparisonOperator_EQ,
        ComparisonOperator_GE,
        ComparisonOperator_GT,
        ComparisonOperator_IN,
        ComparisonOperator_LE,
        ComparisonOperator_LT,
        ComparisonOperator_NE,
        ComparisonOperator_NOT_CONTAINS
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

pattern ComparisonOperator_BEGINS_WITH :: ComparisonOperator
pattern ComparisonOperator_BEGINS_WITH = ComparisonOperator' "BEGINS_WITH"

pattern ComparisonOperator_BETWEEN :: ComparisonOperator
pattern ComparisonOperator_BETWEEN = ComparisonOperator' "BETWEEN"

pattern ComparisonOperator_CONTAINS :: ComparisonOperator
pattern ComparisonOperator_CONTAINS = ComparisonOperator' "CONTAINS"

pattern ComparisonOperator_EQ :: ComparisonOperator
pattern ComparisonOperator_EQ = ComparisonOperator' "EQ"

pattern ComparisonOperator_GE :: ComparisonOperator
pattern ComparisonOperator_GE = ComparisonOperator' "GE"

pattern ComparisonOperator_GT :: ComparisonOperator
pattern ComparisonOperator_GT = ComparisonOperator' "GT"

pattern ComparisonOperator_IN :: ComparisonOperator
pattern ComparisonOperator_IN = ComparisonOperator' "IN"

pattern ComparisonOperator_LE :: ComparisonOperator
pattern ComparisonOperator_LE = ComparisonOperator' "LE"

pattern ComparisonOperator_LT :: ComparisonOperator
pattern ComparisonOperator_LT = ComparisonOperator' "LT"

pattern ComparisonOperator_NE :: ComparisonOperator
pattern ComparisonOperator_NE = ComparisonOperator' "NE"

pattern ComparisonOperator_NOT_CONTAINS :: ComparisonOperator
pattern ComparisonOperator_NOT_CONTAINS = ComparisonOperator' "NOT_CONTAINS"

{-# COMPLETE
  ComparisonOperator_BEGINS_WITH,
  ComparisonOperator_BETWEEN,
  ComparisonOperator_CONTAINS,
  ComparisonOperator_EQ,
  ComparisonOperator_GE,
  ComparisonOperator_GT,
  ComparisonOperator_IN,
  ComparisonOperator_LE,
  ComparisonOperator_LT,
  ComparisonOperator_NE,
  ComparisonOperator_NOT_CONTAINS,
  ComparisonOperator'
  #-}
