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
-- Module      : Network.AWS.Lightsail.GetCloudFormationStackRecords
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the CloudFormation stack record created as a result of the @create cloud formation stack@ operation.
--
--
-- An AWS CloudFormation stack is used to create a new Amazon EC2 instance from an exported Lightsail snapshot.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetCloudFormationStackRecords
    (
    -- * Creating a Request
      getCloudFormationStackRecords
    , GetCloudFormationStackRecords
    -- * Request Lenses
    , gcfsrPageToken

    -- * Destructuring the Response
    , getCloudFormationStackRecordsResponse
    , GetCloudFormationStackRecordsResponse
    -- * Response Lenses
    , gcfsrrsNextPageToken
    , gcfsrrsCloudFormationStackRecords
    , gcfsrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCloudFormationStackRecords' smart constructor.
newtype GetCloudFormationStackRecords = GetCloudFormationStackRecords'
  { _gcfsrPageToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCloudFormationStackRecords' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfsrPageToken' - A token used for advancing to a specific page of results for your @get cloud formation stack records@ request.
getCloudFormationStackRecords
    :: GetCloudFormationStackRecords
getCloudFormationStackRecords =
  GetCloudFormationStackRecords' {_gcfsrPageToken = Nothing}


-- | A token used for advancing to a specific page of results for your @get cloud formation stack records@ request.
gcfsrPageToken :: Lens' GetCloudFormationStackRecords (Maybe Text)
gcfsrPageToken = lens _gcfsrPageToken (\ s a -> s{_gcfsrPageToken = a})

instance AWSPager GetCloudFormationStackRecords where
        page rq rs
          | stop (rs ^. gcfsrrsNextPageToken) = Nothing
          | stop (rs ^. gcfsrrsCloudFormationStackRecords) =
            Nothing
          | otherwise =
            Just $ rq &
              gcfsrPageToken .~ rs ^. gcfsrrsNextPageToken

instance AWSRequest GetCloudFormationStackRecords
         where
        type Rs GetCloudFormationStackRecords =
             GetCloudFormationStackRecordsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetCloudFormationStackRecordsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "cloudFormationStackRecords" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetCloudFormationStackRecords where

instance NFData GetCloudFormationStackRecords where

instance ToHeaders GetCloudFormationStackRecords
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetCloudFormationStackRecords"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCloudFormationStackRecords where
        toJSON GetCloudFormationStackRecords'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gcfsrPageToken])

instance ToPath GetCloudFormationStackRecords where
        toPath = const "/"

instance ToQuery GetCloudFormationStackRecords where
        toQuery = const mempty

-- | /See:/ 'getCloudFormationStackRecordsResponse' smart constructor.
data GetCloudFormationStackRecordsResponse = GetCloudFormationStackRecordsResponse'
  { _gcfsrrsNextPageToken              :: !(Maybe Text)
  , _gcfsrrsCloudFormationStackRecords :: !(Maybe [CloudFormationStackRecord])
  , _gcfsrrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCloudFormationStackRecordsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfsrrsNextPageToken' - A token used for advancing to the next page of results of your get relational database bundles request.
--
-- * 'gcfsrrsCloudFormationStackRecords' - A list of objects describing the CloudFormation stack records.
--
-- * 'gcfsrrsResponseStatus' - -- | The response status code.
getCloudFormationStackRecordsResponse
    :: Int -- ^ 'gcfsrrsResponseStatus'
    -> GetCloudFormationStackRecordsResponse
getCloudFormationStackRecordsResponse pResponseStatus_ =
  GetCloudFormationStackRecordsResponse'
    { _gcfsrrsNextPageToken = Nothing
    , _gcfsrrsCloudFormationStackRecords = Nothing
    , _gcfsrrsResponseStatus = pResponseStatus_
    }


-- | A token used for advancing to the next page of results of your get relational database bundles request.
gcfsrrsNextPageToken :: Lens' GetCloudFormationStackRecordsResponse (Maybe Text)
gcfsrrsNextPageToken = lens _gcfsrrsNextPageToken (\ s a -> s{_gcfsrrsNextPageToken = a})

-- | A list of objects describing the CloudFormation stack records.
gcfsrrsCloudFormationStackRecords :: Lens' GetCloudFormationStackRecordsResponse [CloudFormationStackRecord]
gcfsrrsCloudFormationStackRecords = lens _gcfsrrsCloudFormationStackRecords (\ s a -> s{_gcfsrrsCloudFormationStackRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
gcfsrrsResponseStatus :: Lens' GetCloudFormationStackRecordsResponse Int
gcfsrrsResponseStatus = lens _gcfsrrsResponseStatus (\ s a -> s{_gcfsrrsResponseStatus = a})

instance NFData GetCloudFormationStackRecordsResponse
         where
