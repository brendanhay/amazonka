{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ComparisonOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.ComparisonOperator
  ( ComparisonOperator
    ( ComparisonOperator'
    , ComparisonOperatorEQ
    , ComparisonOperatorNE
    , ComparisonOperatorLE
    , ComparisonOperatorLT
    , ComparisonOperatorGE
    , ComparisonOperatorGT
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

pattern ComparisonOperatorEQ :: ComparisonOperator
pattern ComparisonOperatorEQ = ComparisonOperator' "EQ"

pattern ComparisonOperatorNE :: ComparisonOperator
pattern ComparisonOperatorNE = ComparisonOperator' "NE"

pattern ComparisonOperatorLE :: ComparisonOperator
pattern ComparisonOperatorLE = ComparisonOperator' "LE"

pattern ComparisonOperatorLT :: ComparisonOperator
pattern ComparisonOperatorLT = ComparisonOperator' "LT"

pattern ComparisonOperatorGE :: ComparisonOperator
pattern ComparisonOperatorGE = ComparisonOperator' "GE"

pattern ComparisonOperatorGT :: ComparisonOperator
pattern ComparisonOperatorGT = ComparisonOperator' "GT"

{-# COMPLETE 
  ComparisonOperatorEQ,

  ComparisonOperatorNE,

  ComparisonOperatorLE,

  ComparisonOperatorLT,

  ComparisonOperatorGE,

  ComparisonOperatorGT,
  ComparisonOperator'
  #-}
