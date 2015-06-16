{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
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

-- | Returns a description of specified virtual tapes in the virtual tape
-- shelf (VTS).
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a
-- description of all virtual tapes found in the VTS associated with your
-- account.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapeArchives.html>
module Network.AWS.StorageGateway.DescribeTapeArchives
    (
    -- * Request
      DescribeTapeArchives
    -- ** Request constructor
    , describeTapeArchives
    -- ** Request lenses
    , dtaMarker
    , dtaLimit
    , dtaTapeARNs

    -- * Response
    , DescribeTapeArchivesResponse
    -- ** Response constructor
    , describeTapeArchivesResponse
    -- ** Response lenses
    , dtarTapeArchives
    , dtarMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeTapeArchives' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaMarker'
--
-- * 'dtaLimit'
--
-- * 'dtaTapeARNs'
data DescribeTapeArchives = DescribeTapeArchives'{_dtaMarker :: Maybe Text, _dtaLimit :: Maybe Nat, _dtaTapeARNs :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'DescribeTapeArchives' smart constructor.
describeTapeArchives :: DescribeTapeArchives
describeTapeArchives = DescribeTapeArchives'{_dtaMarker = Nothing, _dtaLimit = Nothing, _dtaTapeARNs = Nothing};

-- | An opaque string that indicates the position at which to begin
-- describing virtual tapes.
dtaMarker :: Lens' DescribeTapeArchives (Maybe Text)
dtaMarker = lens _dtaMarker (\ s a -> s{_dtaMarker = a});

-- | Specifies that the number of virtual tapes descried be limited to the
-- specified number.
dtaLimit :: Lens' DescribeTapeArchives (Maybe Natural)
dtaLimit = lens _dtaLimit (\ s a -> s{_dtaLimit = a}) . mapping _Nat;

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent
-- the virtual tapes you want to describe.
dtaTapeARNs :: Lens' DescribeTapeArchives [Text]
dtaTapeARNs = lens _dtaTapeARNs (\ s a -> s{_dtaTapeARNs = a}) . _Default;

instance AWSRequest DescribeTapeArchives where
        type Sv DescribeTapeArchives = StorageGateway
        type Rs DescribeTapeArchives =
             DescribeTapeArchivesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapeArchivesResponse' <$>
                   (x .?> "TapeArchives" .!@ mempty) <*>
                     (x .?> "Marker"))

instance ToHeaders DescribeTapeArchives where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeTapeArchives" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTapeArchives where
        toJSON DescribeTapeArchives'{..}
          = object
              ["Marker" .= _dtaMarker, "Limit" .= _dtaLimit,
               "TapeARNs" .= _dtaTapeARNs]

instance ToPath DescribeTapeArchives where
        toPath = const "/"

instance ToQuery DescribeTapeArchives where
        toQuery = const mempty

-- | /See:/ 'describeTapeArchivesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtarTapeArchives'
--
-- * 'dtarMarker'
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'{_dtarTapeArchives :: Maybe [TapeArchive], _dtarMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeTapeArchivesResponse' smart constructor.
describeTapeArchivesResponse :: DescribeTapeArchivesResponse
describeTapeArchivesResponse = DescribeTapeArchivesResponse'{_dtarTapeArchives = Nothing, _dtarMarker = Nothing};

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The
-- description includes of the Amazon Resource Name(ARN) of the virtual
-- tapes. The information returned includes the Amazon Resource Names
-- (ARNs) of the tapes, size of the tapes, status of the tapes, progress of
-- the description and tape barcode.
dtarTapeArchives :: Lens' DescribeTapeArchivesResponse [TapeArchive]
dtarTapeArchives = lens _dtarTapeArchives (\ s a -> s{_dtarTapeArchives = a}) . _Default;

-- | An opaque string that indicates the position at which the virtual tapes
-- that were fetched for description ended. Use this marker in your next
-- request to fetch the next set of virtual tapes in the virtual tape shelf
-- (VTS). If there are no more virtual tapes to describe, this field does
-- not appear in the response.
dtarMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtarMarker = lens _dtarMarker (\ s a -> s{_dtarMarker = a});
