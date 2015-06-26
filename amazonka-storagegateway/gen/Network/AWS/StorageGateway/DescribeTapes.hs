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
    , dtMarker
    , dtLimit
    , dtTapeARNs
    , dtGatewayARN

    -- * Response
    , DescribeTapesResponse
    -- ** Response constructor
    , describeTapesResponse
    -- ** Response lenses
    , desMarker
    , desTapes
    , desStatusCode
    ) where

import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | DescribeTapesInput
--
-- /See:/ 'describeTapes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtMarker'
--
-- * 'dtLimit'
--
-- * 'dtTapeARNs'
--
-- * 'dtGatewayARN'
data DescribeTapes = DescribeTapes'{_dtMarker :: Maybe Text, _dtLimit :: Maybe Nat, _dtTapeARNs :: Maybe [Text], _dtGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTapes' smart constructor.
describeTapes :: Text -> DescribeTapes
describeTapes pGatewayARN = DescribeTapes'{_dtMarker = Nothing, _dtLimit = Nothing, _dtTapeARNs = Nothing, _dtGatewayARN = pGatewayARN};

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
dtMarker :: Lens' DescribeTapes (Maybe Text)
dtMarker = lens _dtMarker (\ s a -> s{_dtMarker = a});

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
dtLimit :: Lens' DescribeTapes (Maybe Natural)
dtLimit = lens _dtLimit (\ s a -> s{_dtLimit = a}) . mapping _Nat;

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, AWS Storage Gateway returns a description of all virtual
-- tapes associated with the specified gateway.
dtTapeARNs :: Lens' DescribeTapes [Text]
dtTapeARNs = lens _dtTapeARNs (\ s a -> s{_dtTapeARNs = a}) . _Default;

-- | FIXME: Undocumented member.
dtGatewayARN :: Lens' DescribeTapes Text
dtGatewayARN = lens _dtGatewayARN (\ s a -> s{_dtGatewayARN = a});

instance AWSPager DescribeTapes where
        page rq rs
          | stop (rs ^. desMarker) = Nothing
          | stop (rs ^. desTapes) = Nothing
          | otherwise = Just $ rq & dtMarker .~ rs ^. desMarker

instance AWSRequest DescribeTapes where
        type Sv DescribeTapes = StorageGateway
        type Rs DescribeTapes = DescribeTapesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapesResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Tapes" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
              ["Marker" .= _dtMarker, "Limit" .= _dtLimit,
               "TapeARNs" .= _dtTapeARNs,
               "GatewayARN" .= _dtGatewayARN]

instance ToPath DescribeTapes where
        toPath = const "/"

instance ToQuery DescribeTapes where
        toQuery = const mempty

-- | DescribeTapesOutput
--
-- /See:/ 'describeTapesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desMarker'
--
-- * 'desTapes'
--
-- * 'desStatusCode'
data DescribeTapesResponse = DescribeTapesResponse'{_desMarker :: Maybe Text, _desTapes :: Maybe [Tape], _desStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeTapesResponse' smart constructor.
describeTapesResponse :: Int -> DescribeTapesResponse
describeTapesResponse pStatusCode = DescribeTapesResponse'{_desMarker = Nothing, _desTapes = Nothing, _desStatusCode = pStatusCode};

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
desMarker :: Lens' DescribeTapesResponse (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a});

-- | An array of virtual tape descriptions.
desTapes :: Lens' DescribeTapesResponse [Tape]
desTapes = lens _desTapes (\ s a -> s{_desTapes = a}) . _Default;

-- | FIXME: Undocumented member.
desStatusCode :: Lens' DescribeTapesResponse Int
desStatusCode = lens _desStatusCode (\ s a -> s{_desStatusCode = a});
