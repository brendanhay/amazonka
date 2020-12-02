{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionList where

import Network.AWS.CloudFront.Types.StreamingDistributionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A streaming distribution list.
--
--
--
-- /See:/ 'streamingDistributionList' smart constructor.
data StreamingDistributionList = StreamingDistributionList'
  { _sdlItems ::
      !(Maybe [StreamingDistributionSummary]),
    _sdlNextMarker :: !(Maybe Text),
    _sdlMarker :: !Text,
    _sdlMaxItems :: !Int,
    _sdlIsTruncated :: !Bool,
    _sdlQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamingDistributionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdlItems' - A complex type that contains one @StreamingDistributionSummary@ element for each distribution that was created by the current AWS account.
--
-- * 'sdlNextMarker' - If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your RTMP distributions where they left off.
--
-- * 'sdlMarker' - The value you provided for the @Marker@ request parameter.
--
-- * 'sdlMaxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- * 'sdlIsTruncated' - A flag that indicates whether more streaming distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
--
-- * 'sdlQuantity' - The number of streaming distributions that were created by the current AWS account.
streamingDistributionList ::
  -- | 'sdlMarker'
  Text ->
  -- | 'sdlMaxItems'
  Int ->
  -- | 'sdlIsTruncated'
  Bool ->
  -- | 'sdlQuantity'
  Int ->
  StreamingDistributionList
streamingDistributionList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    StreamingDistributionList'
      { _sdlItems = Nothing,
        _sdlNextMarker = Nothing,
        _sdlMarker = pMarker_,
        _sdlMaxItems = pMaxItems_,
        _sdlIsTruncated = pIsTruncated_,
        _sdlQuantity = pQuantity_
      }

-- | A complex type that contains one @StreamingDistributionSummary@ element for each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList [StreamingDistributionSummary]
sdlItems = lens _sdlItems (\s a -> s {_sdlItems = a}) . _Default . _Coerce

-- | If @IsTruncated@ is @true@ , this element is present and contains the value you can use for the @Marker@ request parameter to continue listing your RTMP distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\s a -> s {_sdlNextMarker = a})

-- | The value you provided for the @Marker@ request parameter.
sdlMarker :: Lens' StreamingDistributionList Text
sdlMarker = lens _sdlMarker (\s a -> s {_sdlMarker = a})

-- | The value you provided for the @MaxItems@ request parameter.
sdlMaxItems :: Lens' StreamingDistributionList Int
sdlMaxItems = lens _sdlMaxItems (\s a -> s {_sdlMaxItems = a})

-- | A flag that indicates whether more streaming distributions remain to be listed. If your results were truncated, you can make a follow-up pagination request using the @Marker@ request parameter to retrieve more distributions in the list.
sdlIsTruncated :: Lens' StreamingDistributionList Bool
sdlIsTruncated = lens _sdlIsTruncated (\s a -> s {_sdlIsTruncated = a})

-- | The number of streaming distributions that were created by the current AWS account.
sdlQuantity :: Lens' StreamingDistributionList Int
sdlQuantity = lens _sdlQuantity (\s a -> s {_sdlQuantity = a})

instance FromXML StreamingDistributionList where
  parseXML x =
    StreamingDistributionList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "StreamingDistributionSummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "Marker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Quantity")

instance Hashable StreamingDistributionList

instance NFData StreamingDistributionList
