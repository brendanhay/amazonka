{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListUploads
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about uploads.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListUploads.html>
module Network.AWS.DeviceFarm.ListUploads
    (
    -- * Request
      ListUploads
    -- ** Request constructor
    , listUploads
    -- ** Request lenses
    , lurqNextToken
    , lurqArn

    -- * Response
    , ListUploadsResponse
    -- ** Response constructor
    , listUploadsResponse
    -- ** Response lenses
    , lursNextToken
    , lursUploads
    , lursStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list uploads operation.
--
-- /See:/ 'listUploads' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lurqNextToken'
--
-- * 'lurqArn'
data ListUploads = ListUploads'
    { _lurqNextToken :: !(Maybe Text)
    , _lurqArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUploads' smart constructor.
listUploads :: Text -> ListUploads
listUploads pArn_ =
    ListUploads'
    { _lurqNextToken = Nothing
    , _lurqArn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lurqNextToken :: Lens' ListUploads (Maybe Text)
lurqNextToken = lens _lurqNextToken (\ s a -> s{_lurqNextToken = a});

-- | The uploads\' ARNs.
lurqArn :: Lens' ListUploads Text
lurqArn = lens _lurqArn (\ s a -> s{_lurqArn = a});

instance AWSRequest ListUploads where
        type Sv ListUploads = DeviceFarm
        type Rs ListUploads = ListUploadsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListUploadsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "uploads" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListUploads where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListUploads" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUploads where
        toJSON ListUploads'{..}
          = object
              ["nextToken" .= _lurqNextToken, "arn" .= _lurqArn]

instance ToPath ListUploads where
        toPath = const "/"

instance ToQuery ListUploads where
        toQuery = const mempty

-- | Represents the result of a list uploads request.
--
-- /See:/ 'listUploadsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lursNextToken'
--
-- * 'lursUploads'
--
-- * 'lursStatus'
data ListUploadsResponse = ListUploadsResponse'
    { _lursNextToken :: !(Maybe Text)
    , _lursUploads   :: !(Maybe [Upload])
    , _lursStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUploadsResponse' smart constructor.
listUploadsResponse :: Int -> ListUploadsResponse
listUploadsResponse pStatus_ =
    ListUploadsResponse'
    { _lursNextToken = Nothing
    , _lursUploads = Nothing
    , _lursStatus = pStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lursNextToken :: Lens' ListUploadsResponse (Maybe Text)
lursNextToken = lens _lursNextToken (\ s a -> s{_lursNextToken = a});

-- | Information about the uploads.
lursUploads :: Lens' ListUploadsResponse [Upload]
lursUploads = lens _lursUploads (\ s a -> s{_lursUploads = a}) . _Default;

-- | FIXME: Undocumented member.
lursStatus :: Lens' ListUploadsResponse Int
lursStatus = lens _lursStatus (\ s a -> s{_lursStatus = a});
