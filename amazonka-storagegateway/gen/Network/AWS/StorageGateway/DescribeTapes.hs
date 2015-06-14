{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a description of the specified Amazon Resource Name (ARN) of
-- virtual tapes. If a @TapeARN@ is not specified, returns a description of
-- all virtual tapes associated with the specified gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapes.html>
module Network.AWS.StorageGateway.DescribeTapes
    (
    -- * Request
      DescribeTapes
    -- ** Request constructor
    , describeTapes
    -- ** Request lenses
    , dtTapeARNs
    , dtGatewayARN
    , dtMarker
    , dtLimit

    -- * Response
    , DescribeTapesResponse
    -- ** Response constructor
    , describeTapesResponse
    -- ** Response lenses
    , dtrTapes
    , dtrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeTapes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTapeARNs'
--
-- * 'dtGatewayARN'
--
-- * 'dtMarker'
--
-- * 'dtLimit'
data DescribeTapes = DescribeTapes'{_dtTapeARNs :: [Text], _dtGatewayARN :: Text, _dtMarker :: Text, _dtLimit :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeTapes' smart constructor.
describeTapes :: Text -> Text -> Natural -> DescribeTapes
describeTapes pGatewayARN pMarker pLimit = DescribeTapes'{_dtTapeARNs = mempty, _dtGatewayARN = pGatewayARN, _dtMarker = pMarker, _dtLimit = _Nat # pLimit};

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, AWS Storage Gateway returns a description of all virtual
-- tapes associated with the specified gateway.
dtTapeARNs :: Lens' DescribeTapes [Text]
dtTapeARNs = lens _dtTapeARNs (\ s a -> s{_dtTapeARNs = a});

-- | FIXME: Undocumented member.
dtGatewayARN :: Lens' DescribeTapes Text
dtGatewayARN = lens _dtGatewayARN (\ s a -> s{_dtGatewayARN = a});

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
dtMarker :: Lens' DescribeTapes Text
dtMarker = lens _dtMarker (\ s a -> s{_dtMarker = a});

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
dtLimit :: Lens' DescribeTapes Natural
dtLimit = lens _dtLimit (\ s a -> s{_dtLimit = a}) . _Nat;

instance AWSRequest DescribeTapes where
        type Sv DescribeTapes = StorageGateway
        type Rs DescribeTapes = DescribeTapesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapesResponse' <$>
                   x .?> "Tapes" .!@ mempty <*> x .:> "Marker")

instance ToHeaders DescribeTapes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeTapes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTapes where
        toJSON DescribeTapes'{..}
          = object
              ["TapeARNs" .= _dtTapeARNs,
               "GatewayARN" .= _dtGatewayARN, "Marker" .= _dtMarker,
               "Limit" .= _dtLimit]

instance ToPath DescribeTapes where
        toPath = const "/"

instance ToQuery DescribeTapes where
        toQuery = const mempty

-- | /See:/ 'describeTapesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTapes'
--
-- * 'dtrMarker'
data DescribeTapesResponse = DescribeTapesResponse'{_dtrTapes :: [Tape], _dtrMarker :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTapesResponse' smart constructor.
describeTapesResponse :: Text -> DescribeTapesResponse
describeTapesResponse pMarker = DescribeTapesResponse'{_dtrTapes = mempty, _dtrMarker = pMarker};

-- | An array of virtual tape descriptions.
dtrTapes :: Lens' DescribeTapesResponse [Tape]
dtrTapes = lens _dtrTapes (\ s a -> s{_dtrTapes = a});

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
dtrMarker :: Lens' DescribeTapesResponse Text
dtrMarker = lens _dtrMarker (\ s a -> s{_dtrMarker = a});
