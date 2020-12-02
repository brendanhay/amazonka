{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists the origin access identities for CloudFront.Send a @GET@ request to the @//CloudFront API version/ /origin-access-identity/cloudfront@ resource. The response includes a @CloudFrontOriginAccessIdentityList@ element with zero or more @CloudFrontOriginAccessIdentitySummary@ child elements. By default, your entire list of origin access identities is returned in one single page. If the list is long, you can paginate it using the @MaxItems@ and @Marker@ parameters.
--
--
--
-- /See:/ 'cloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
  { _cfoailItems ::
      !( Maybe
           [CloudFrontOriginAccessIdentitySummary]
       ),
    _cfoailNextMarker ::
      !(Maybe Text),
    _cfoailMarker ::
      !Text,
    _cfoailMaxItems ::
      !Int,
    _cfoailIsTruncated ::
      !Bool,
    _cfoailQuantity ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFrontOriginAccessIdentityList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfoailItems' - A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
--
-- * 'cfoailNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
--
-- * 'cfoailMarker' - Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
--
-- * 'cfoailMaxItems' - The maximum number of origin access identities you want in the response body.
--
-- * 'cfoailIsTruncated' - A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
--
-- * 'cfoailQuantity' - The number of CloudFront origin access identities that were created by the current AWS account.
cloudFrontOriginAccessIdentityList ::
  -- | 'cfoailMarker'
  Text ->
  -- | 'cfoailMaxItems'
  Int ->
  -- | 'cfoailIsTruncated'
  Bool ->
  -- | 'cfoailQuantity'
  Int ->
  CloudFrontOriginAccessIdentityList
cloudFrontOriginAccessIdentityList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    CloudFrontOriginAccessIdentityList'
      { _cfoailItems = Nothing,
        _cfoailNextMarker = Nothing,
        _cfoailMarker = pMarker_,
        _cfoailMaxItems = pMaxItems_,
        _cfoailIsTruncated = pIsTruncated_,
        _cfoailQuantity = pQuantity_
      }

-- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@ element for each origin access identity that was created by the current AWS account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList [CloudFrontOriginAccessIdentitySummary]
cfoailItems = lens _cfoailItems (\s a -> s {_cfoailItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your origin access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\s a -> s {_cfoailNextMarker = a})

-- | Use this when paginating results to indicate where to begin in your list of origin access identities. The results include identities in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last identity on that page).
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList Text
cfoailMarker = lens _cfoailMarker (\s a -> s {_cfoailMarker = a})

-- | The maximum number of origin access identities you want in the response body.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailMaxItems = lens _cfoailMaxItems (\s a -> s {_cfoailMaxItems = a})

-- | A flag that indicates whether more origin access identities remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more items in the list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList Bool
cfoailIsTruncated = lens _cfoailIsTruncated (\s a -> s {_cfoailIsTruncated = a})

-- | The number of CloudFront origin access identities that were created by the current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailQuantity = lens _cfoailQuantity (\s a -> s {_cfoailQuantity = a})

instance FromXML CloudFrontOriginAccessIdentityList where
  parseXML x =
    CloudFrontOriginAccessIdentityList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "CloudFrontOriginAccessIdentitySummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "Marker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Quantity")

instance Hashable CloudFrontOriginAccessIdentityList

instance NFData CloudFrontOriginAccessIdentityList
