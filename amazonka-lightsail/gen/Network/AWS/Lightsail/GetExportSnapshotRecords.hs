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
-- Module      : Network.AWS.Lightsail.GetExportSnapshotRecords
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the export snapshot record created as a result of the @export snapshot@ operation.
--
--
-- An export snapshot record can be used to create a new Amazon EC2 instance and its related resources with the @create cloud formation stack@ operation.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetExportSnapshotRecords
    (
    -- * Creating a Request
      getExportSnapshotRecords
    , GetExportSnapshotRecords
    -- * Request Lenses
    , gesrPageToken

    -- * Destructuring the Response
    , getExportSnapshotRecordsResponse
    , GetExportSnapshotRecordsResponse
    -- * Response Lenses
    , gesrrsNextPageToken
    , gesrrsExportSnapshotRecords
    , gesrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getExportSnapshotRecords' smart constructor.
newtype GetExportSnapshotRecords = GetExportSnapshotRecords'
  { _gesrPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExportSnapshotRecords' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesrPageToken' - A token used for advancing to a specific page of results for your @get export snapshot records@ request.
getExportSnapshotRecords
    :: GetExportSnapshotRecords
getExportSnapshotRecords = GetExportSnapshotRecords' {_gesrPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get export snapshot records@ request.
gesrPageToken :: Lens' GetExportSnapshotRecords (Maybe Text)
gesrPageToken = lens _gesrPageToken (\ s a -> s{_gesrPageToken = a})

instance AWSPager GetExportSnapshotRecords where
        page rq rs
          | stop (rs ^. gesrrsNextPageToken) = Nothing
          | stop (rs ^. gesrrsExportSnapshotRecords) = Nothing
          | otherwise =
            Just $ rq &
              gesrPageToken .~ rs ^. gesrrsNextPageToken

instance AWSRequest GetExportSnapshotRecords where
        type Rs GetExportSnapshotRecords =
             GetExportSnapshotRecordsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetExportSnapshotRecordsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "exportSnapshotRecords" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetExportSnapshotRecords where

instance NFData GetExportSnapshotRecords where

instance ToHeaders GetExportSnapshotRecords where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetExportSnapshotRecords" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetExportSnapshotRecords where
        toJSON GetExportSnapshotRecords'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gesrPageToken])

instance ToPath GetExportSnapshotRecords where
        toPath = const "/"

instance ToQuery GetExportSnapshotRecords where
        toQuery = const mempty

-- | /See:/ 'getExportSnapshotRecordsResponse' smart constructor.
data GetExportSnapshotRecordsResponse = GetExportSnapshotRecordsResponse'
  { _gesrrsNextPageToken         :: !(Maybe Text)
  , _gesrrsExportSnapshotRecords :: !(Maybe [ExportSnapshotRecord])
  , _gesrrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExportSnapshotRecordsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gesrrsNextPageToken' - A token used for advancing to the next page of results of your get relational database bundles request.
--
-- * 'gesrrsExportSnapshotRecords' - A list of objects describing the export snapshot records.
--
-- * 'gesrrsResponseStatus' - -- | The response status code.
getExportSnapshotRecordsResponse
    :: Int -- ^ 'gesrrsResponseStatus'
    -> GetExportSnapshotRecordsResponse
getExportSnapshotRecordsResponse pResponseStatus_ =
  GetExportSnapshotRecordsResponse'
    { _gesrrsNextPageToken = Nothing
    , _gesrrsExportSnapshotRecords = Nothing
    , _gesrrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results of your get relational database bundles request.
gesrrsNextPageToken :: Lens' GetExportSnapshotRecordsResponse (Maybe Text)
gesrrsNextPageToken = lens _gesrrsNextPageToken (\ s a -> s{_gesrrsNextPageToken = a})

-- | A list of objects describing the export snapshot records.
gesrrsExportSnapshotRecords :: Lens' GetExportSnapshotRecordsResponse [ExportSnapshotRecord]
gesrrsExportSnapshotRecords = lens _gesrrsExportSnapshotRecords (\ s a -> s{_gesrrsExportSnapshotRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
gesrrsResponseStatus :: Lens' GetExportSnapshotRecordsResponse Int
gesrrsResponseStatus = lens _gesrrsResponseStatus (\ s a -> s{_gesrrsResponseStatus = a})

instance NFData GetExportSnapshotRecordsResponse
         where
