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
-- Module      : Network.AWS.EMR.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_GREATER_THAN,
        ComparisonOperator_GREATER_THAN_OR_EQUAL,
        ComparisonOperator_LESS_THAN,
        ComparisonOperator_LESS_THAN_OR_EQUAL
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

pattern ComparisonOperator_GREATER_THAN :: ComparisonOperator
pattern ComparisonOperator_GREATER_THAN = ComparisonOperator' "GREATER_THAN"

pattern ComparisonOperator_GREATER_THAN_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_GREATER_THAN_OR_EQUAL = ComparisonOperator' "GREATER_THAN_OR_EQUAL"

pattern ComparisonOperator_LESS_THAN :: ComparisonOperator
pattern ComparisonOperator_LESS_THAN = ComparisonOperator' "LESS_THAN"

pattern ComparisonOperator_LESS_THAN_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_LESS_THAN_OR_EQUAL = ComparisonOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  ComparisonOperator_GREATER_THAN,
  ComparisonOperator_GREATER_THAN_OR_EQUAL,
  ComparisonOperator_LESS_THAN,
  ComparisonOperator_LESS_THAN_OR_EQUAL,
  ComparisonOperator'
  #-}
