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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ComparisonOperator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_EQUALS,
        ComparisonOperator_NOT_EQUALS,
        ComparisonOperator_STARTS_WITH
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

pattern ComparisonOperator_EQUALS :: ComparisonOperator
pattern ComparisonOperator_EQUALS = ComparisonOperator' "EQUALS"

pattern ComparisonOperator_NOT_EQUALS :: ComparisonOperator
pattern ComparisonOperator_NOT_EQUALS = ComparisonOperator' "NOT_EQUALS"

pattern ComparisonOperator_STARTS_WITH :: ComparisonOperator
pattern ComparisonOperator_STARTS_WITH = ComparisonOperator' "STARTS_WITH"

{-# COMPLETE
  ComparisonOperator_EQUALS,
  ComparisonOperator_NOT_EQUALS,
  ComparisonOperator_STARTS_WITH,
  ComparisonOperator'
  #-}
