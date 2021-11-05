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
-- Module      : Network.AWS.IoTEvents.Types.ComparisonOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.ComparisonOperator
  ( ComparisonOperator
      ( ..,
        ComparisonOperator_EQUAL,
        ComparisonOperator_GREATER,
        ComparisonOperator_GREATER_OR_EQUAL,
        ComparisonOperator_LESS,
        ComparisonOperator_LESS_OR_EQUAL,
        ComparisonOperator_NOT_EQUAL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ComparisonOperator = ComparisonOperator'
  { fromComparisonOperator ::
      Core.Text
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

pattern ComparisonOperator_EQUAL :: ComparisonOperator
pattern ComparisonOperator_EQUAL = ComparisonOperator' "EQUAL"

pattern ComparisonOperator_GREATER :: ComparisonOperator
pattern ComparisonOperator_GREATER = ComparisonOperator' "GREATER"

pattern ComparisonOperator_GREATER_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_GREATER_OR_EQUAL = ComparisonOperator' "GREATER_OR_EQUAL"

pattern ComparisonOperator_LESS :: ComparisonOperator
pattern ComparisonOperator_LESS = ComparisonOperator' "LESS"

pattern ComparisonOperator_LESS_OR_EQUAL :: ComparisonOperator
pattern ComparisonOperator_LESS_OR_EQUAL = ComparisonOperator' "LESS_OR_EQUAL"

pattern ComparisonOperator_NOT_EQUAL :: ComparisonOperator
pattern ComparisonOperator_NOT_EQUAL = ComparisonOperator' "NOT_EQUAL"

{-# COMPLETE
  ComparisonOperator_EQUAL,
  ComparisonOperator_GREATER,
  ComparisonOperator_GREATER_OR_EQUAL,
  ComparisonOperator_LESS,
  ComparisonOperator_LESS_OR_EQUAL,
  ComparisonOperator_NOT_EQUAL,
  ComparisonOperator'
  #-}
