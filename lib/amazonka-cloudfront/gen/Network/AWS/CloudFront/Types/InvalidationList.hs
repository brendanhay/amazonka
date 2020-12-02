{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationList where

import Network.AWS.CloudFront.Types.InvalidationSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @InvalidationList@ complex type describes the list of invalidation objects. For more information about invalidation, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html Invalidating Objects (Web Distributions Only)> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'invalidationList' smart constructor.
data InvalidationList = InvalidationList'
  { _ilItems ::
      !(Maybe [InvalidationSummary]),
    _ilNextMarker :: !(Maybe Text),
    _ilMarker :: !Text,
    _ilMaxItems :: !Int,
    _ilIsTruncated :: !Bool,
    _ilQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InvalidationList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilItems' - A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
--
-- * 'ilNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
--
-- * 'ilMarker' - The value that you provided for the @Marker@ request parameter.
--
-- * 'ilMaxItems' - The value that you provided for the @MaxItems@ request parameter.
--
-- * 'ilIsTruncated' - A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
--
-- * 'ilQuantity' - The number of invalidation batches that were created by the current AWS account.
invalidationList ::
  -- | 'ilMarker'
  Text ->
  -- | 'ilMaxItems'
  Int ->
  -- | 'ilIsTruncated'
  Bool ->
  -- | 'ilQuantity'
  Int ->
  InvalidationList
invalidationList pMarker_ pMaxItems_ pIsTruncated_ pQuantity_ =
  InvalidationList'
    { _ilItems = Nothing,
      _ilNextMarker = Nothing,
      _ilMarker = pMarker_,
      _ilMaxItems = pMaxItems_,
      _ilIsTruncated = pIsTruncated_,
      _ilQuantity = pQuantity_
    }

-- | A complex type that contains one @InvalidationSummary@ element for each invalidation batch created by the current AWS account.
ilItems :: Lens' InvalidationList [InvalidationSummary]
ilItems = lens _ilItems (\s a -> s {_ilItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\s a -> s {_ilNextMarker = a})

-- | The value that you provided for the @Marker@ request parameter.
ilMarker :: Lens' InvalidationList Text
ilMarker = lens _ilMarker (\s a -> s {_ilMarker = a})

-- | The value that you provided for the @MaxItems@ request parameter.
ilMaxItems :: Lens' InvalidationList Int
ilMaxItems = lens _ilMaxItems (\s a -> s {_ilMaxItems = a})

-- | A flag that indicates whether more invalidation batch requests remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more invalidation batches in the list.
ilIsTruncated :: Lens' InvalidationList Bool
ilIsTruncated = lens _ilIsTruncated (\s a -> s {_ilIsTruncated = a})

-- | The number of invalidation batches that were created by the current AWS account.
ilQuantity :: Lens' InvalidationList Int
ilQuantity = lens _ilQuantity (\s a -> s {_ilQuantity = a})

instance FromXML InvalidationList where
  parseXML x =
    InvalidationList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "InvalidationSummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "Marker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Quantity")

instance Hashable InvalidationList

instance NFData InvalidationList
