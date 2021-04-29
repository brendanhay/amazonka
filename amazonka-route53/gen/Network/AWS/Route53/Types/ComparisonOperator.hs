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
-- Module      : Network.AWS.Route53.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_GreaterThanOrEqualToThreshold,
        ComparisonOperator_GreaterThanThreshold,
        ComparisonOperator_LessThanOrEqualToThreshold,
        ComparisonOperator_LessThanThreshold
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

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
