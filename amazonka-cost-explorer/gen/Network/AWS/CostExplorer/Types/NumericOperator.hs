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
-- Module      : Network.AWS.CostExplorer.Types.NumericOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.NumericOperator
  ( NumericOperator
      ( ..,
        NumericOperator_BETWEEN,
        NumericOperator_EQUAL,
        NumericOperator_GREATER_THAN,
        NumericOperator_GREATER_THAN_OR_EQUAL,
        NumericOperator_LESS_THAN,
        NumericOperator_LESS_THAN_OR_EQUAL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype NumericOperator = NumericOperator'
  { fromNumericOperator ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern NumericOperator_BETWEEN :: NumericOperator
pattern NumericOperator_BETWEEN = NumericOperator' "BETWEEN"

pattern NumericOperator_EQUAL :: NumericOperator
pattern NumericOperator_EQUAL = NumericOperator' "EQUAL"

pattern NumericOperator_GREATER_THAN :: NumericOperator
pattern NumericOperator_GREATER_THAN = NumericOperator' "GREATER_THAN"

pattern NumericOperator_GREATER_THAN_OR_EQUAL :: NumericOperator
pattern NumericOperator_GREATER_THAN_OR_EQUAL = NumericOperator' "GREATER_THAN_OR_EQUAL"

pattern NumericOperator_LESS_THAN :: NumericOperator
pattern NumericOperator_LESS_THAN = NumericOperator' "LESS_THAN"

pattern NumericOperator_LESS_THAN_OR_EQUAL :: NumericOperator
pattern NumericOperator_LESS_THAN_OR_EQUAL = NumericOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  NumericOperator_BETWEEN,
  NumericOperator_EQUAL,
  NumericOperator_GREATER_THAN,
  NumericOperator_GREATER_THAN_OR_EQUAL,
  NumericOperator_LESS_THAN,
  NumericOperator_LESS_THAN_OR_EQUAL,
  NumericOperator'
  #-}
