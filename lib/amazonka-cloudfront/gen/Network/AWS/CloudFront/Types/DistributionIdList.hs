{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionIdList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionIdList where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of distribution IDs.
--
--
--
-- /See:/ 'distributionIdList' smart constructor.
data DistributionIdList = DistributionIdList'
  { _dilItems ::
      !(Maybe [Text]),
    _dilNextMarker :: !(Maybe Text),
    _dilMarker :: !Text,
    _dilMaxItems :: !Int,
    _dilIsTruncated :: !Bool,
    _dilQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionIdList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dilItems' - Contains the distribution IDs in the list.
--
-- * 'dilNextMarker' - Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
--
-- * 'dilMarker' - The value provided in the @Marker@ request field.
--
-- * 'dilMaxItems' - The maximum number of distribution IDs requested.
--
-- * 'dilIsTruncated' - A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
--
-- * 'dilQuantity' - The total number of distribution IDs returned in the response.
distributionIdList ::
  -- | 'dilMarker'
  Text ->
  -- | 'dilMaxItems'
  Int ->
  -- | 'dilIsTruncated'
  Bool ->
  -- | 'dilQuantity'
  Int ->
  DistributionIdList
distributionIdList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
  DistributionIdList'
    { _dilItems = Nothing,
      _dilNextMarker = Nothing,
      _dilMarker = pMarker_,
      _dilMaxItems = pMaxItems_,
      _dilIsTruncated = pIsTruncated_,
      _dilQuantity = pQuantity_
    }

-- | Contains the distribution IDs in the list.
dilItems :: Lens' DistributionIdList [Text]
dilItems = lens _dilItems (\s a -> s {_dilItems = a}) . _Default . _Coerce

-- | Contains the value that you should use in the @Marker@ field of a subsequent request to continue listing distribution IDs where you left off.
dilNextMarker :: Lens' DistributionIdList (Maybe Text)
dilNextMarker = lens _dilNextMarker (\s a -> s {_dilNextMarker = a})

-- | The value provided in the @Marker@ request field.
dilMarker :: Lens' DistributionIdList Text
dilMarker = lens _dilMarker (\s a -> s {_dilMarker = a})

-- | The maximum number of distribution IDs requested.
dilMaxItems :: Lens' DistributionIdList Int
dilMaxItems = lens _dilMaxItems (\s a -> s {_dilMaxItems = a})

-- | A flag that indicates whether more distribution IDs remain to be listed. If your results were truncated, you can make a subsequent request using the @Marker@ request field to retrieve more distribution IDs in the list.
dilIsTruncated :: Lens' DistributionIdList Bool
dilIsTruncated = lens _dilIsTruncated (\s a -> s {_dilIsTruncated = a})

-- | The total number of distribution IDs returned in the response.
dilQuantity :: Lens' DistributionIdList Int
dilQuantity = lens _dilQuantity (\s a -> s {_dilQuantity = a})

instance FromXML DistributionIdList where
  parseXML x =
    DistributionIdList'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "DistributionId"))
      <*> (x .@? "NextMarker")
      <*> (x .@ "Marker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Quantity")

instance Hashable DistributionIdList

instance NFData DistributionIdList
