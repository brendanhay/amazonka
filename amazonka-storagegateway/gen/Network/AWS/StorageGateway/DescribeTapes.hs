{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified Amazon Resource Name (ARN) of
-- virtual tapes. If a 'TapeARN' is not specified, returns a description of
-- all virtual tapes associated with the specified gateway.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapes.html AWS API Reference> for DescribeTapes.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapes
    (
    -- * Creating a Request
      describeTapes
    , DescribeTapes
    -- * Request Lenses
    , dtMarker
    , dtLimit
    , dtTapeARNs
    , dtGatewayARN

    -- * Destructuring the Response
    , describeTapesResponse
    , DescribeTapesResponse
    -- * Response Lenses
    , dtsrsMarker
    , dtsrsTapes
    , dtsrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | DescribeTapesInput
--
-- /See:/ 'describeTapes' smart constructor.
data DescribeTapes = DescribeTapes'
    { _dtMarker     :: !(Maybe Text)
    , _dtLimit      :: !(Maybe Nat)
    , _dtTapeARNs   :: !(Maybe [Text])
    , _dtGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTapes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtMarker'
--
-- * 'dtLimit'
--
-- * 'dtTapeARNs'
--
-- * 'dtGatewayARN'
describeTapes
    :: Text -- ^ 'dtGatewayARN'
    -> DescribeTapes
describeTapes pGatewayARN_ =
    DescribeTapes'
    { _dtMarker = Nothing
    , _dtLimit = Nothing
    , _dtTapeARNs = Nothing
    , _dtGatewayARN = pGatewayARN_
    }

-- | A marker value, obtained in a previous call to 'DescribeTapes'. This
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
dtTapeARNs = lens _dtTapeARNs (\ s a -> s{_dtTapeARNs = a}) . _Default . _Coerce;

-- | Undocumented member.
dtGatewayARN :: Lens' DescribeTapes Text
dtGatewayARN = lens _dtGatewayARN (\ s a -> s{_dtGatewayARN = a});

instance AWSPager DescribeTapes where
        page rq rs
          | stop (rs ^. dtsrsMarker) = Nothing
          | stop (rs ^. dtsrsTapes) = Nothing
          | otherwise =
            Just $ rq & dtMarker .~ rs ^. dtsrsMarker

instance AWSRequest DescribeTapes where
        type Rs DescribeTapes = DescribeTapesResponse
        request = postJSON storageGateway
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
              (catMaybes
                 [("Marker" .=) <$> _dtMarker,
                  ("Limit" .=) <$> _dtLimit,
                  ("TapeARNs" .=) <$> _dtTapeARNs,
                  Just ("GatewayARN" .= _dtGatewayARN)])

instance ToPath DescribeTapes where
        toPath = const "/"

instance ToQuery DescribeTapes where
        toQuery = const mempty

-- | DescribeTapesOutput
--
-- /See:/ 'describeTapesResponse' smart constructor.
data DescribeTapesResponse = DescribeTapesResponse'
    { _dtsrsMarker :: !(Maybe Text)
    , _dtsrsTapes  :: !(Maybe [Tape])
    , _dtsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTapesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsrsMarker'
--
-- * 'dtsrsTapes'
--
-- * 'dtsrsStatus'
describeTapesResponse
    :: Int -- ^ 'dtsrsStatus'
    -> DescribeTapesResponse
describeTapesResponse pStatus_ =
    DescribeTapesResponse'
    { _dtsrsMarker = Nothing
    , _dtsrsTapes = Nothing
    , _dtsrsStatus = pStatus_
    }

-- | An opaque string which can be used as part of a subsequent DescribeTapes
-- call to retrieve the next page of results.
--
-- If a response does not contain a marker, then there are no more results
-- to be retrieved.
dtsrsMarker :: Lens' DescribeTapesResponse (Maybe Text)
dtsrsMarker = lens _dtsrsMarker (\ s a -> s{_dtsrsMarker = a});

-- | An array of virtual tape descriptions.
dtsrsTapes :: Lens' DescribeTapesResponse [Tape]
dtsrsTapes = lens _dtsrsTapes (\ s a -> s{_dtsrsTapes = a}) . _Default . _Coerce;

-- | The response status code.
dtsrsStatus :: Lens' DescribeTapesResponse Int
dtsrsStatus = lens _dtsrsStatus (\ s a -> s{_dtsrsStatus = a});
