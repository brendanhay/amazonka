{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
  ( ReturnConsumedCapacity
    ( ReturnConsumedCapacity'
    , ReturnConsumedCapacityIndexes
    , ReturnConsumedCapacityTotal
    , ReturnConsumedCapacityNone
    , fromReturnConsumedCapacity
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Determines the level of detail about provisioned throughput consumption that is returned in the response:
--
--
--     * @INDEXES@ - The response includes the aggregate @ConsumedCapacity@ for the operation, together with @ConsumedCapacity@ for each table and secondary index that was accessed.
-- Note that some operations, such as @GetItem@ and @BatchGetItem@ , do not access any indexes at all. In these cases, specifying @INDEXES@ will only return @ConsumedCapacity@ information for table(s).
--
--
--     * @TOTAL@ - The response includes only the aggregate @ConsumedCapacity@ for the operation.
--
--
--     * @NONE@ - No @ConsumedCapacity@ details are included in the response.
--
--
newtype ReturnConsumedCapacity = ReturnConsumedCapacity'{fromReturnConsumedCapacity
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern ReturnConsumedCapacityIndexes :: ReturnConsumedCapacity
pattern ReturnConsumedCapacityIndexes = ReturnConsumedCapacity' "INDEXES"

pattern ReturnConsumedCapacityTotal :: ReturnConsumedCapacity
pattern ReturnConsumedCapacityTotal = ReturnConsumedCapacity' "TOTAL"

pattern ReturnConsumedCapacityNone :: ReturnConsumedCapacity
pattern ReturnConsumedCapacityNone = ReturnConsumedCapacity' "NONE"

{-# COMPLETE 
  ReturnConsumedCapacityIndexes,

  ReturnConsumedCapacityTotal,

  ReturnConsumedCapacityNone,
  ReturnConsumedCapacity'
  #-}
