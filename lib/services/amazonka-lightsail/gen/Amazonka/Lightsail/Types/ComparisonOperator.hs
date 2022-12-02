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
-- Module      : Amazonka.Lightsail.Types.ComparisonOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_GreaterThanOrEqualToThreshold,
        ComparisonOperator_GreaterThanThreshold,
        ComparisonOperator_LessThanOrEqualToThreshold,
        ComparisonOperator_LessThanThreshold
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

pattern ComparisonOperator_GreaterThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperator_GreaterThanOrEqualToThreshold = ComparisonOperator' "GreaterThanOrEqualToThreshold"

pattern ComparisonOperator_GreaterThanThreshold :: ComparisonOperator
pattern ComparisonOperator_GreaterThanThreshold = ComparisonOperator' "GreaterThanThreshold"

pattern ComparisonOperator_LessThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanOrEqualToThreshold = ComparisonOperator' "LessThanOrEqualToThreshold"

pattern ComparisonOperator_LessThanThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanThreshold = ComparisonOperator' "LessThanThreshold"

{-# COMPLETE
  ComparisonOperator_GreaterThanOrEqualToThreshold,
  ComparisonOperator_GreaterThanThreshold,
  ComparisonOperator_LessThanOrEqualToThreshold,
  ComparisonOperator_LessThanThreshold,
  ComparisonOperator'
  #-}
