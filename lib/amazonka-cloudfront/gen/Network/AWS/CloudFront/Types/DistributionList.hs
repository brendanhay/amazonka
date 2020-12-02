{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.DistributionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.DistributionList where

import Network.AWS.CloudFront.Types.DistributionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A distribution list.
--
--
--
-- /See:/ 'distributionList' smart constructor.
data DistributionList = DistributionList'
  { _dlItems ::
      !(Maybe [DistributionSummary]),
    _dlNextMarker :: !(Maybe Text),
    _dlMarker :: !Text,
    _dlMaxItems :: !Int,
    _dlIsTruncated :: !Bool,
    _dlQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlItems' - A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- * 'dlNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
--
-- * 'dlMarker' - The value you provided for the @Marker@ request parameter.
--
-- * 'dlMaxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- * 'dlIsTruncated' - A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- * 'dlQuantity' - The number of distributions that were created by the current AWS account.
distributionList ::
  -- | 'dlMarker'
  Text ->
  -- | 'dlMaxItems'
  Int ->
  -- | 'dlIsTruncated'
  Bool ->
  -- | 'dlQuantity'
  Int ->
  DistributionList
distributionList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
  DistributionList'
    { _dlItems = Nothing,
      _dlNextMarker = Nothing,
      _dlMarker = pMarker_,
      _dlMaxItems = pMaxItems_,
      _dlIsTruncated = pIsTruncated_,
      _dlQuantity = pQuantity_
    }

-- | A complex type that contains one @DistributionSummary@ element for each distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList [DistributionSummary]
dlItems = lens _dlItems (\s a -> s {_dlItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\s a -> s {_dlNextMarker = a})

-- | The value you provided for the @Marker@ request parameter.
dlMarker :: Lens' DistributionList Text
dlMarker = lens _dlMarker (\s a -> s {_dlMarker = a})

-- | The value you provided for the @MaxItems@ request parameter.
dlMaxItems :: Lens' DistributionList Int
dlMaxItems = lens _dlMaxItems (\s a -> s {_dlMaxItems = a})

-- | A flag that indicates whether more distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
dlIsTruncated :: Lens' DistributionList Bool
dlIsTruncated = lens _dlIsTruncated (\s a -> s {_dlIsTruncated = a})

-- | The number of distributions that were created by the current AWS account.
dlQuantity :: Lens' DistributionList Int
dlQuantity = lens _dlQuantity (\s a -> s {_dlQuantity = a})

instance FromXML DistributionList where
  parseXML x =
    DistributionList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "DistributionSummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "Marker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Quantity")

instance Hashable DistributionList

instance NFData DistributionList
