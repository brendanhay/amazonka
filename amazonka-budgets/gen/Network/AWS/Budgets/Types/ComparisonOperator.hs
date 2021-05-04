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

import qualified Network.AWS.Prelude as Prelude

-- | The comparison operator of a notification. Currently the service
-- supports the following operators:
--
-- @GREATER_THAN@, @LESS_THAN@, @EQUAL_TO@
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
