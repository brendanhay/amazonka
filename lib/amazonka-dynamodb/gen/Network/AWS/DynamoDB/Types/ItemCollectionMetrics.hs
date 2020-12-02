{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ItemCollectionMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ItemCollectionMetrics where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about item collections, if any, that were affected by the operation. @ItemCollectionMetrics@ is only returned if the request asked for it. If the table does not have any local secondary indexes, this information is not returned in the response.
--
--
--
-- /See:/ 'itemCollectionMetrics' smart constructor.
data ItemCollectionMetrics = ItemCollectionMetrics'
  { _icmItemCollectionKey ::
      !(Maybe (Map Text (AttributeValue))),
    _icmSizeEstimateRangeGB :: !(Maybe [Double])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ItemCollectionMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icmItemCollectionKey' - The partition key value of the item collection. This value is the same as the partition key value of the item.
--
-- * 'icmSizeEstimateRangeGB' - An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
itemCollectionMetrics ::
  ItemCollectionMetrics
itemCollectionMetrics =
  ItemCollectionMetrics'
    { _icmItemCollectionKey = Nothing,
      _icmSizeEstimateRangeGB = Nothing
    }

-- | The partition key value of the item collection. This value is the same as the partition key value of the item.
icmItemCollectionKey :: Lens' ItemCollectionMetrics (HashMap Text (AttributeValue))
icmItemCollectionKey = lens _icmItemCollectionKey (\s a -> s {_icmItemCollectionKey = a}) . _Default . _Map

-- | An estimate of item collection size, in gigabytes. This value is a two-element array containing a lower bound and an upper bound for the estimate. The estimate includes the size of all the items in the table, plus the size of all attributes projected into all of the local secondary indexes on that table. Use this estimate to measure whether a local secondary index is approaching its size limit. The estimate is subject to change over time; therefore, do not rely on the precision or accuracy of the estimate.
icmSizeEstimateRangeGB :: Lens' ItemCollectionMetrics [Double]
icmSizeEstimateRangeGB = lens _icmSizeEstimateRangeGB (\s a -> s {_icmSizeEstimateRangeGB = a}) . _Default . _Coerce

instance FromJSON ItemCollectionMetrics where
  parseJSON =
    withObject
      "ItemCollectionMetrics"
      ( \x ->
          ItemCollectionMetrics'
            <$> (x .:? "ItemCollectionKey" .!= mempty)
            <*> (x .:? "SizeEstimateRangeGB" .!= mempty)
      )

instance Hashable ItemCollectionMetrics

instance NFData ItemCollectionMetrics
