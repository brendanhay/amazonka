{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ComparisonOperator
  ( ComparisonOperator
    ( ComparisonOperator'
    , ComparisonOperatorGreaterThanOrEqualToThreshold
    , ComparisonOperatorGreaterThanThreshold
    , ComparisonOperatorLessThanThreshold
    , ComparisonOperatorLessThanOrEqualToThreshold
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

pattern ComparisonOperatorGreaterThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperatorGreaterThanOrEqualToThreshold = ComparisonOperator' "GreaterThanOrEqualToThreshold"

pattern ComparisonOperatorGreaterThanThreshold :: ComparisonOperator
pattern ComparisonOperatorGreaterThanThreshold = ComparisonOperator' "GreaterThanThreshold"

pattern ComparisonOperatorLessThanThreshold :: ComparisonOperator
pattern ComparisonOperatorLessThanThreshold = ComparisonOperator' "LessThanThreshold"

pattern ComparisonOperatorLessThanOrEqualToThreshold :: ComparisonOperator
pattern ComparisonOperatorLessThanOrEqualToThreshold = ComparisonOperator' "LessThanOrEqualToThreshold"

{-# COMPLETE 
  ComparisonOperatorGreaterThanOrEqualToThreshold,

  ComparisonOperatorGreaterThanThreshold,

  ComparisonOperatorLessThanThreshold,

  ComparisonOperatorLessThanOrEqualToThreshold,
  ComparisonOperator'
  #-}
