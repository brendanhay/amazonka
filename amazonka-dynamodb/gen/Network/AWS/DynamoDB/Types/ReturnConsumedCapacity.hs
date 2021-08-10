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
-- Module      : Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
  ( ReturnConsumedCapacity
      ( ..,
        ReturnConsumedCapacity_INDEXES,
        ReturnConsumedCapacity_NONE,
        ReturnConsumedCapacity_TOTAL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Determines the level of detail about provisioned throughput consumption
-- that is returned in the response:
--
-- -   @INDEXES@ - The response includes the aggregate @ConsumedCapacity@
--     for the operation, together with @ConsumedCapacity@ for each table
--     and secondary index that was accessed.
--
--     Note that some operations, such as @GetItem@ and @BatchGetItem@, do
--     not access any indexes at all. In these cases, specifying @INDEXES@
--     will only return @ConsumedCapacity@ information for table(s).
--
-- -   @TOTAL@ - The response includes only the aggregate
--     @ConsumedCapacity@ for the operation.
--
-- -   @NONE@ - No @ConsumedCapacity@ details are included in the response.
newtype ReturnConsumedCapacity = ReturnConsumedCapacity'
  { fromReturnConsumedCapacity ::
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

pattern ReturnConsumedCapacity_INDEXES :: ReturnConsumedCapacity
pattern ReturnConsumedCapacity_INDEXES = ReturnConsumedCapacity' "INDEXES"

pattern ReturnConsumedCapacity_NONE :: ReturnConsumedCapacity
pattern ReturnConsumedCapacity_NONE = ReturnConsumedCapacity' "NONE"

pattern ReturnConsumedCapacity_TOTAL :: ReturnConsumedCapacity
pattern ReturnConsumedCapacity_TOTAL = ReturnConsumedCapacity' "TOTAL"

{-# COMPLETE
  ReturnConsumedCapacity_INDEXES,
  ReturnConsumedCapacity_NONE,
  ReturnConsumedCapacity_TOTAL,
  ReturnConsumedCapacity'
  #-}
