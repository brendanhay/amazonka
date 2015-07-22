{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified Amazon Resource Name (ARN) of
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
    , dtrqMarker
    , dtrqLimit
    , dtrqTapeARNs
    , dtrqGatewayARN

    -- * Response
    , DescribeTapesResponse
    -- ** Response constructor
    , describeTapesResponse
    -- ** Response lenses
    , dtrsMarker
    , dtrsTapes
    , dtrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DescribeTapesInput
--
-- /See:/ 'describeTapes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrqMarker'
--
-- * 'dtrqLimit'
--
-- * 'dtrqTapeARNs'
--
-- * 'dtrqGatewayARN'
data DescribeTapes = DescribeTapes'
    { _dtrqMarker     :: !(Maybe Text)
    , _dtrqLimit      :: !(Maybe Nat)
    , _dtrqTapeARNs   :: !(Maybe [Text])
    , _dtrqGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapes' smart constructor.
describeTapes :: Text -> DescribeTapes
describeTapes pGatewayARN =
    DescribeTapes'
    { _dtrqMarker = Nothing
    , _dtrqLimit = Nothing
    , _dtrqTapeARNs = Nothing
    , _dtrqGatewayARN = pGatewayARN
    }

-- | A marker value, obtained in a previous call to @DescribeTapes@. This
-- marker indicates which page of results to retrieve.
--
-- If not specified, the first page of results is retrieved.
dtrqMarker :: Lens' DescribeTapes (Maybe Text)
dtrqMarker = lens _dtrqMarker (\ s a -> s{_dtrqMarker = a});

-- | Specifies that the number of virtual tapes described be limited to the
-- specified number.
--
-- Amazon Web Services may impose its own limit, if this field is not set.
dtrqLimit :: Lens' DescribeTapes (Maybe Natural)
dtrqLimit = lens _dtrqLimit (\ s a -> s{_dtrqLimit = a}) . mapping _Nat;

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe. If this parameter is not
-- specified, AWS Storage Gateway returns a description of all virtual
-- tapes associated with the specified gateway.
dtrqTapeARNs :: Lens' DescribeTapes [Text]
dtrqTapeARNs = lens _dtrqTapeARNs (\ s a -> s{_dtrqTapeARNs = a}) . _Default;

-- | FIXME: Undocumented member.
dtrqGatewayARN :: Lens' DescribeTapes Text
dtrqGatewayARN = lens _dtrqGatewayARN (\ s a -> s{_dtrqGatewayARN = a});

instance AWSPager DescribeTapes where
        page rq rs
          | stop (rs ^. dtrsMarker) = Nothing
          | stop (rs ^. dtrsTapes) = Nothing
          | otherwise =
            Just $ rq & dtrqMarker .~ rs ^. dtrsMarker

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
              ["Marker" .= _dtrqMarker, "Limit" .= _dtrqLimit,
               "TapeARNs" .= _dtrqTapeARNs,
               "GatewayARN" .= _dtrqGatewayARN]

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
-- * 'dtrsMarker'
--
-- * 'dtrsTapes'
--
-- * 'dtrsStatus'
data DescribeTapesResponse = DescribeTapesResponse'
    { _dtrsMarker :: !(Maybe Text)
    , _dtrsTapes  :: !(Maybe [Tape])
    , _dtrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapesResponse' smart constructor.
describeTapesResponse :: Int -> DescribeTapesResponse
describeTapesResponse pStatus =
    DescribeTapesResponse'
    { _dtrsMarker = Nothing
    , _dtrsTapes = Nothing
    , _dtrsStatus = pStatus
    }

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
dtrsMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtrsMarker = lens _dtrsMarker (\ s a -> s{_dtrsMarker = a});

-- | An array of virtual tape descriptions.
dtrsTapes :: Lens' DescribeTapesResponse [Tape]
dtrsTapes = lens _dtrsTapes (\ s a -> s{_dtrsTapes = a}) . _Default;

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTapesResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
