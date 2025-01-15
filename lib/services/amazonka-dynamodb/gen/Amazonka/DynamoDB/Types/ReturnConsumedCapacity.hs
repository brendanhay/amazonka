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
-- Module      : Amazonka.DynamoDB.Types.ReturnConsumedCapacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReturnConsumedCapacity
  ( ReturnConsumedCapacity
      ( ..,
        ReturnConsumedCapacity_INDEXES,
        ReturnConsumedCapacity_NONE,
        ReturnConsumedCapacity_TOTAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Determines the level of detail about either provisioned or on-demand
-- throughput consumption that is returned in the response:
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
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
