{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of specified virtual tapes in the virtual tape
-- shelf (VTS).
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a
-- description of all virtual tapes found in the VTS associated with your
-- account.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeTapeArchives.html AWS API Reference> for DescribeTapeArchives.
module Network.AWS.StorageGateway.DescribeTapeArchives
    (
    -- * Creating a Request
      DescribeTapeArchives
    , describeTapeArchives
    -- * Request Lenses
    , dtaMarker
    , dtaLimit
    , dtaTapeARNs

    -- * Destructuring the Response
    , DescribeTapeArchivesResponse
    , describeTapeArchivesResponse
    -- * Response Lenses
    , dtarsTapeArchives
    , dtarsMarker
    , dtarsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DescribeTapeArchivesInput
--
-- /See:/ 'describeTapeArchives' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaMarker'
--
-- * 'dtaLimit'
--
-- * 'dtaTapeARNs'
data DescribeTapeArchives = DescribeTapeArchives'
    { _dtaMarker   :: !(Maybe Text)
    , _dtaLimit    :: !(Maybe Nat)
    , _dtaTapeARNs :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapeArchives' smart constructor.
describeTapeArchives :: DescribeTapeArchives
describeTapeArchives =
    DescribeTapeArchives'
    { _dtaMarker = Nothing
    , _dtaLimit = Nothing
    , _dtaTapeARNs = Nothing
    }

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
dtaTapeARNs = lens _dtaTapeARNs (\ s a -> s{_dtaTapeARNs = a}) . _Default . _Coerce;

instance AWSPager DescribeTapeArchives where
        page rq rs
          | stop (rs ^. dtarsMarker) = Nothing
          | stop (rs ^. dtarsTapeArchives) = Nothing
          | otherwise =
            Just $ rq & dtaMarker .~ rs ^. dtarsMarker

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
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

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

-- | DescribeTapeArchivesOutput
--
-- /See:/ 'describeTapeArchivesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtarsTapeArchives'
--
-- * 'dtarsMarker'
--
-- * 'dtarsStatus'
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'
    { _dtarsTapeArchives :: !(Maybe [TapeArchive])
    , _dtarsMarker       :: !(Maybe Text)
    , _dtarsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTapeArchivesResponse' smart constructor.
describeTapeArchivesResponse :: Int -> DescribeTapeArchivesResponse
describeTapeArchivesResponse pStatus_ =
    DescribeTapeArchivesResponse'
    { _dtarsTapeArchives = Nothing
    , _dtarsMarker = Nothing
    , _dtarsStatus = pStatus_
    }

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The
-- description includes of the Amazon Resource Name(ARN) of the virtual
-- tapes. The information returned includes the Amazon Resource Names
-- (ARNs) of the tapes, size of the tapes, status of the tapes, progress of
-- the description and tape barcode.
dtarsTapeArchives :: Lens' DescribeTapeArchivesResponse [TapeArchive]
dtarsTapeArchives = lens _dtarsTapeArchives (\ s a -> s{_dtarsTapeArchives = a}) . _Default . _Coerce;

-- | An opaque string that indicates the position at which the virtual tapes
-- that were fetched for description ended. Use this marker in your next
-- request to fetch the next set of virtual tapes in the virtual tape shelf
-- (VTS). If there are no more virtual tapes to describe, this field does
-- not appear in the response.
dtarsMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtarsMarker = lens _dtarsMarker (\ s a -> s{_dtarsMarker = a});

-- | Undocumented member.
dtarsStatus :: Lens' DescribeTapeArchivesResponse Int
dtarsStatus = lens _dtarsStatus (\ s a -> s{_dtarsStatus = a});
