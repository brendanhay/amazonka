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
-- Module      : Network.AWS.KinesisAnalytics.ListApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Amazon Kinesis Analytics applications in your account. For each application, the response includes the application name, Amazon Resource Name (ARN), and status. If the response returns the @HasMoreApplications@ value as true, you can send another request by adding the @ExclusiveStartApplicationName@ in the request body, and set the value of this to the last application name from the previous response.
--
--
-- If you want detailed information about a specific application, use 'DescribeApplication' .
--
-- This operation requires permissions to perform the @kinesisanalytics:ListApplications@ action.
--
module Network.AWS.KinesisAnalytics.ListApplications
    (
    -- * Creating a Request
      listApplications
    , ListApplications
    -- * Request Lenses
    , laLimit
    , laExclusiveStartApplicationName

    -- * Destructuring the Response
    , listApplicationsResponse
    , ListApplicationsResponse
    -- * Response Lenses
    , larsResponseStatus
    , larsApplicationSummaries
    , larsHasMoreApplications
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'listApplications' smart constructor.
data ListApplications = ListApplications'
  { _laLimit                         :: !(Maybe Nat)
  , _laExclusiveStartApplicationName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laLimit' - Maximum number of applications to list.
--
-- * 'laExclusiveStartApplicationName' - Name of the application to start the list with. When using pagination to retrieve the list, you don't need to specify this parameter in the first request. However, in subsequent requests, you add the last application name from the previous response to get the next page of applications.
listApplications
    :: ListApplications
listApplications =
  ListApplications'
    {_laLimit = Nothing, _laExclusiveStartApplicationName = Nothing}


-- | Maximum number of applications to list.
laLimit :: Lens' ListApplications (Maybe Natural)
laLimit = lens _laLimit (\ s a -> s{_laLimit = a}) . mapping _Nat

-- | Name of the application to start the list with. When using pagination to retrieve the list, you don't need to specify this parameter in the first request. However, in subsequent requests, you add the last application name from the previous response to get the next page of applications.
laExclusiveStartApplicationName :: Lens' ListApplications (Maybe Text)
laExclusiveStartApplicationName = lens _laExclusiveStartApplicationName (\ s a -> s{_laExclusiveStartApplicationName = a})

instance AWSRequest ListApplications where
        type Rs ListApplications = ListApplicationsResponse
        request = postJSON kinesisAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "ApplicationSummaries" .!@ mempty)
                     <*> (x .:> "HasMoreApplications"))

instance Hashable ListApplications where

instance NFData ListApplications where

instance ToHeaders ListApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.ListApplications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListApplications where
        toJSON ListApplications'{..}
          = object
              (catMaybes
                 [("Limit" .=) <$> _laLimit,
                  ("ExclusiveStartApplicationName" .=) <$>
                    _laExclusiveStartApplicationName])

instance ToPath ListApplications where
        toPath = const "/"

instance ToQuery ListApplications where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'listApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { _larsResponseStatus       :: !Int
  , _larsApplicationSummaries :: ![ApplicationSummary]
  , _larsHasMoreApplications  :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsResponseStatus' - -- | The response status code.
--
-- * 'larsApplicationSummaries' - List of @ApplicationSummary@ objects.
--
-- * 'larsHasMoreApplications' - Returns true if there are more applications to retrieve.
listApplicationsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> Bool -- ^ 'larsHasMoreApplications'
    -> ListApplicationsResponse
listApplicationsResponse pResponseStatus_ pHasMoreApplications_ =
  ListApplicationsResponse'
    { _larsResponseStatus = pResponseStatus_
    , _larsApplicationSummaries = mempty
    , _larsHasMoreApplications = pHasMoreApplications_
    }


-- | -- | The response status code.
larsResponseStatus :: Lens' ListApplicationsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

-- | List of @ApplicationSummary@ objects.
larsApplicationSummaries :: Lens' ListApplicationsResponse [ApplicationSummary]
larsApplicationSummaries = lens _larsApplicationSummaries (\ s a -> s{_larsApplicationSummaries = a}) . _Coerce

-- | Returns true if there are more applications to retrieve.
larsHasMoreApplications :: Lens' ListApplicationsResponse Bool
larsHasMoreApplications = lens _larsHasMoreApplications (\ s a -> s{_larsHasMoreApplications = a})

instance NFData ListApplicationsResponse where
