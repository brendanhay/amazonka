{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ComparisonOperator
  ( ComparisonOperator
    ( ComparisonOperator'
    , ComparisonOperatorGreaterThanOrEqual
    , ComparisonOperatorGreaterThan
    , ComparisonOperatorLessThan
    , ComparisonOperatorLessThanOrEqual
    , fromComparisonOperator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ComparisonOperator = ComparisonOperator'{fromComparisonOperator
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ComparisonOperatorGreaterThanOrEqual :: ComparisonOperator
pattern ComparisonOperatorGreaterThanOrEqual = ComparisonOperator' "GREATER_THAN_OR_EQUAL"

pattern ComparisonOperatorGreaterThan :: ComparisonOperator
pattern ComparisonOperatorGreaterThan = ComparisonOperator' "GREATER_THAN"

pattern ComparisonOperatorLessThan :: ComparisonOperator
pattern ComparisonOperatorLessThan = ComparisonOperator' "LESS_THAN"

pattern ComparisonOperatorLessThanOrEqual :: ComparisonOperator
pattern ComparisonOperatorLessThanOrEqual = ComparisonOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE 
  ComparisonOperatorGreaterThanOrEqual,

  ComparisonOperatorGreaterThan,

  ComparisonOperatorLessThan,

  ComparisonOperatorLessThanOrEqual,
  ComparisonOperator'
  #-}
