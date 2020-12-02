{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InventoryRetrievalJobDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the options for a range inventory retrieval job.
--
--
--
-- /See:/ 'inventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { _irjdFormat ::
      !(Maybe Text),
    _irjdEndDate ::
      !(Maybe Text),
    _irjdStartDate ::
      !(Maybe Text),
    _irjdMarker ::
      !(Maybe Text),
    _irjdLimit ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InventoryRetrievalJobDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irjdFormat' - The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
--
-- * 'irjdEndDate' - The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjdStartDate' - The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
--
-- * 'irjdMarker' - An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
--
-- * 'irjdLimit' - The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
inventoryRetrievalJobDescription ::
  InventoryRetrievalJobDescription
inventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { _irjdFormat = Nothing,
      _irjdEndDate = Nothing,
      _irjdStartDate = Nothing,
      _irjdMarker = Nothing,
      _irjdLimit = Nothing
    }

-- | The output format for the vault inventory list, which is set by the __InitiateJob__ request when initiating a job to retrieve a vault inventory. Valid values are @CSV@ and @JSON@ .
irjdFormat :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdFormat = lens _irjdFormat (\s a -> s {_irjdFormat = a})

-- | The end of the date range in UTC for vault inventory retrieval that includes archives created before this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjdEndDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdEndDate = lens _irjdEndDate (\s a -> s {_irjdEndDate = a})

-- | The start of the date range in Universal Coordinated Time (UTC) for vault inventory retrieval that includes archives created on or after this date. This value should be a string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@ .
irjdStartDate :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdStartDate = lens _irjdStartDate (\s a -> s {_irjdStartDate = a})

-- | An opaque string that represents where to continue pagination of the vault inventory retrieval results. You use the marker in a new __InitiateJob__ request to obtain additional inventory items. If there are no more inventory items, this value is @null@ . For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval> .
irjdMarker :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdMarker = lens _irjdMarker (\s a -> s {_irjdMarker = a})

-- | The maximum number of inventory items returned per vault inventory retrieval request. This limit is set when initiating the job with the a __InitiateJob__ request.
irjdLimit :: Lens' InventoryRetrievalJobDescription (Maybe Text)
irjdLimit = lens _irjdLimit (\s a -> s {_irjdLimit = a})

instance FromJSON InventoryRetrievalJobDescription where
  parseJSON =
    withObject
      "InventoryRetrievalJobDescription"
      ( \x ->
          InventoryRetrievalJobDescription'
            <$> (x .:? "Format")
            <*> (x .:? "EndDate")
            <*> (x .:? "StartDate")
            <*> (x .:? "Marker")
            <*> (x .:? "Limit")
      )

instance Hashable InventoryRetrievalJobDescription

instance NFData InventoryRetrievalJobDescription
