{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_GreaterThanOrEqualToThreshold,
        ComparisonOperator_GreaterThanThreshold,
        ComparisonOperator_GreaterThanUpperThreshold,
        ComparisonOperator_LessThanLowerOrGreaterThanUpperThreshold,
        ComparisonOperator_LessThanLowerThreshold,
        ComparisonOperator_LessThanOrEqualToThreshold,
        ComparisonOperator_LessThanThreshold
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ComparisonOperator_GreaterThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperator_GreaterThanOrEqualToThreshold = ComparisonOperator' "GreaterThanOrEqualToThreshold"

pattern ComparisonOperator_GreaterThanThreshold :: ComparisonOperator
pattern ComparisonOperator_GreaterThanThreshold = ComparisonOperator' "GreaterThanThreshold"

pattern ComparisonOperator_GreaterThanUpperThreshold :: ComparisonOperator
pattern ComparisonOperator_GreaterThanUpperThreshold = ComparisonOperator' "GreaterThanUpperThreshold"

pattern ComparisonOperator_LessThanLowerOrGreaterThanUpperThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanLowerOrGreaterThanUpperThreshold = ComparisonOperator' "LessThanLowerOrGreaterThanUpperThreshold"

pattern ComparisonOperator_LessThanLowerThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanLowerThreshold = ComparisonOperator' "LessThanLowerThreshold"

pattern ComparisonOperator_LessThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanOrEqualToThreshold = ComparisonOperator' "LessThanOrEqualToThreshold"

pattern ComparisonOperator_LessThanThreshold :: ComparisonOperator
pattern ComparisonOperator_LessThanThreshold = ComparisonOperator' "LessThanThreshold"

{-# COMPLETE
  ComparisonOperator_GreaterThanOrEqualToThreshold,
  ComparisonOperator_GreaterThanThreshold,
  ComparisonOperator_GreaterThanUpperThreshold,
  ComparisonOperator_LessThanLowerOrGreaterThanUpperThreshold,
  ComparisonOperator_LessThanLowerThreshold,
  ComparisonOperator_LessThanOrEqualToThreshold,
  ComparisonOperator_LessThanThreshold,
  ComparisonOperator'
  #-}
