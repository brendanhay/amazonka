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
-- Module      : Network.AWS.Budgets.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_EQUAL_TO,
        ComparisonOperator_GREATER_THAN,
        ComparisonOperator_LESS_THAN
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The comparison operator of a notification. Currently the service
-- supports the following operators:
--
-- @GREATER_THAN@, @LESS_THAN@, @EQUAL_TO@
newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
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

pattern ComparisonOperator_EQUAL_TO :: ComparisonOperator
pattern ComparisonOperator_EQUAL_TO = ComparisonOperator' "EQUAL_TO"

pattern ComparisonOperator_GREATER_THAN :: ComparisonOperator
pattern ComparisonOperator_GREATER_THAN = ComparisonOperator' "GREATER_THAN"

pattern ComparisonOperator_LESS_THAN :: ComparisonOperator
pattern ComparisonOperator_LESS_THAN = ComparisonOperator' "LESS_THAN"

{-# COMPLETE
  ComparisonOperator_EQUAL_TO,
  ComparisonOperator_GREATER_THAN,
  ComparisonOperator_LESS_THAN,
  ComparisonOperator'
  #-}
