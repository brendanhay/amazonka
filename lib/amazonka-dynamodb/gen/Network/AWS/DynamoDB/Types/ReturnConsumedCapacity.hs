-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
  ( ReturnConsumedCapacity
      ( ReturnConsumedCapacity',
        RCCIndexes,
        RCCNone,
        RCCTotal
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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
newtype ReturnConsumedCapacity = ReturnConsumedCapacity' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RCCIndexes :: ReturnConsumedCapacity
pattern RCCIndexes = ReturnConsumedCapacity' "INDEXES"

pattern RCCNone :: ReturnConsumedCapacity
pattern RCCNone = ReturnConsumedCapacity' "NONE"

pattern RCCTotal :: ReturnConsumedCapacity
pattern RCCTotal = ReturnConsumedCapacity' "TOTAL"

{-# COMPLETE
  RCCIndexes,
  RCCNone,
  RCCTotal,
  ReturnConsumedCapacity'
  #-}
