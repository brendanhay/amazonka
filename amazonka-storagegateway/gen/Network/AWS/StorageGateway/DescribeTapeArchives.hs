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
-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
--
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.
--
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeArchives
    (
    -- * Creating a Request
      describeTapeArchives
    , DescribeTapeArchives
    -- * Request Lenses
    , dtaMarker
    , dtaLimit
    , dtaTapeARNs

    -- * Destructuring the Response
    , describeTapeArchivesResponse
    , DescribeTapeArchivesResponse
    -- * Response Lenses
    , dtarsTapeArchives
    , dtarsMarker
    , dtarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | DescribeTapeArchivesInput
--
--
--
-- /See:/ 'describeTapeArchives' smart constructor.
data DescribeTapeArchives = DescribeTapeArchives'
  { _dtaMarker   :: !(Maybe Text)
  , _dtaLimit    :: !(Maybe Nat)
  , _dtaTapeARNs :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTapeArchives' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtaMarker' - An opaque string that indicates the position at which to begin describing virtual tapes.
--
-- * 'dtaLimit' - Specifies that the number of virtual tapes descried be limited to the specified number.
--
-- * 'dtaTapeARNs' - Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
describeTapeArchives
    :: DescribeTapeArchives
describeTapeArchives =
  DescribeTapeArchives'
    {_dtaMarker = Nothing, _dtaLimit = Nothing, _dtaTapeARNs = Nothing}


-- | An opaque string that indicates the position at which to begin describing virtual tapes.
dtaMarker :: Lens' DescribeTapeArchives (Maybe Text)
dtaMarker = lens _dtaMarker (\ s a -> s{_dtaMarker = a})

-- | Specifies that the number of virtual tapes descried be limited to the specified number.
dtaLimit :: Lens' DescribeTapeArchives (Maybe Natural)
dtaLimit = lens _dtaLimit (\ s a -> s{_dtaLimit = a}) . mapping _Nat

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
dtaTapeARNs :: Lens' DescribeTapeArchives [Text]
dtaTapeARNs = lens _dtaTapeARNs (\ s a -> s{_dtaTapeARNs = a}) . _Default . _Coerce

instance AWSPager DescribeTapeArchives where
        page rq rs
          | stop (rs ^. dtarsMarker) = Nothing
          | stop (rs ^. dtarsTapeArchives) = Nothing
          | otherwise =
            Just $ rq & dtaMarker .~ rs ^. dtarsMarker

instance AWSRequest DescribeTapeArchives where
        type Rs DescribeTapeArchives =
             DescribeTapeArchivesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTapeArchivesResponse' <$>
                   (x .?> "TapeArchives" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTapeArchives where

instance NFData DescribeTapeArchives where

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
              (catMaybes
                 [("Marker" .=) <$> _dtaMarker,
                  ("Limit" .=) <$> _dtaLimit,
                  ("TapeARNs" .=) <$> _dtaTapeARNs])

instance ToPath DescribeTapeArchives where
        toPath = const "/"

instance ToQuery DescribeTapeArchives where
        toQuery = const mempty

-- | DescribeTapeArchivesOutput
--
--
--
-- /See:/ 'describeTapeArchivesResponse' smart constructor.
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'
  { _dtarsTapeArchives   :: !(Maybe [TapeArchive])
  , _dtarsMarker         :: !(Maybe Text)
  , _dtarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTapeArchivesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtarsTapeArchives' - An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name(ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description and tape barcode.
--
-- * 'dtarsMarker' - An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
--
-- * 'dtarsResponseStatus' - -- | The response status code.
describeTapeArchivesResponse
    :: Int -- ^ 'dtarsResponseStatus'
    -> DescribeTapeArchivesResponse
describeTapeArchivesResponse pResponseStatus_ =
  DescribeTapeArchivesResponse'
    { _dtarsTapeArchives = Nothing
    , _dtarsMarker = Nothing
    , _dtarsResponseStatus = pResponseStatus_
    }


-- | An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name(ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description and tape barcode.
dtarsTapeArchives :: Lens' DescribeTapeArchivesResponse [TapeArchive]
dtarsTapeArchives = lens _dtarsTapeArchives (\ s a -> s{_dtarsTapeArchives = a}) . _Default . _Coerce

-- | An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
dtarsMarker :: Lens' DescribeTapeArchivesResponse (Maybe Text)
dtarsMarker = lens _dtarsMarker (\ s a -> s{_dtarsMarker = a})

-- | -- | The response status code.
dtarsResponseStatus :: Lens' DescribeTapeArchivesResponse Int
dtarsResponseStatus = lens _dtarsResponseStatus (\ s a -> s{_dtarsResponseStatus = a})

instance NFData DescribeTapeArchivesResponse where
