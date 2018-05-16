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
-- Module      : Network.AWS.Lightsail.GetOperationsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operations for a specific resource (e.g., an instance or a static IP).
--
--
module Network.AWS.Lightsail.GetOperationsForResource
    (
    -- * Creating a Request
      getOperationsForResource
    , GetOperationsForResource
    -- * Request Lenses
    , gofrPageToken
    , gofrResourceName

    -- * Destructuring the Response
    , getOperationsForResourceResponse
    , GetOperationsForResourceResponse
    -- * Response Lenses
    , gofrrsNextPageCount
    , gofrrsNextPageToken
    , gofrrsOperations
    , gofrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOperationsForResource' smart constructor.
data GetOperationsForResource = GetOperationsForResource'
  { _gofrPageToken    :: !(Maybe Text)
  , _gofrResourceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gofrPageToken' - A token used for advancing to the next page of results from your get operations for resource request.
--
-- * 'gofrResourceName' - The name of the resource for which you are requesting information.
getOperationsForResource
    :: Text -- ^ 'gofrResourceName'
    -> GetOperationsForResource
getOperationsForResource pResourceName_ =
  GetOperationsForResource'
    {_gofrPageToken = Nothing, _gofrResourceName = pResourceName_}


-- | A token used for advancing to the next page of results from your get operations for resource request.
gofrPageToken :: Lens' GetOperationsForResource (Maybe Text)
gofrPageToken = lens _gofrPageToken (\ s a -> s{_gofrPageToken = a})

-- | The name of the resource for which you are requesting information.
gofrResourceName :: Lens' GetOperationsForResource Text
gofrResourceName = lens _gofrResourceName (\ s a -> s{_gofrResourceName = a})

instance AWSRequest GetOperationsForResource where
        type Rs GetOperationsForResource =
             GetOperationsForResourceResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetOperationsForResourceResponse' <$>
                   (x .?> "nextPageCount") <*> (x .?> "nextPageToken")
                     <*> (x .?> "operations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetOperationsForResource where

instance NFData GetOperationsForResource where

instance ToHeaders GetOperationsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetOperationsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOperationsForResource where
        toJSON GetOperationsForResource'{..}
          = object
              (catMaybes
                 [("pageToken" .=) <$> _gofrPageToken,
                  Just ("resourceName" .= _gofrResourceName)])

instance ToPath GetOperationsForResource where
        toPath = const "/"

instance ToQuery GetOperationsForResource where
        toQuery = const mempty

-- | /See:/ 'getOperationsForResourceResponse' smart constructor.
data GetOperationsForResourceResponse = GetOperationsForResourceResponse'
  { _gofrrsNextPageCount  :: !(Maybe Text)
  , _gofrrsNextPageToken  :: !(Maybe Text)
  , _gofrrsOperations     :: !(Maybe [Operation])
  , _gofrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetOperationsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gofrrsNextPageCount' - (Deprecated) Returns the number of pages of results that remain.
--
-- * 'gofrrsNextPageToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'gofrrsOperations' - An array of key-value pairs containing information about the results of your get operations for resource request.
--
-- * 'gofrrsResponseStatus' - -- | The response status code.
getOperationsForResourceResponse
    :: Int -- ^ 'gofrrsResponseStatus'
    -> GetOperationsForResourceResponse
getOperationsForResourceResponse pResponseStatus_ =
  GetOperationsForResourceResponse'
    { _gofrrsNextPageCount = Nothing
    , _gofrrsNextPageToken = Nothing
    , _gofrrsOperations = Nothing
    , _gofrrsResponseStatus = pResponseStatus_
    }


-- | (Deprecated) Returns the number of pages of results that remain.
gofrrsNextPageCount :: Lens' GetOperationsForResourceResponse (Maybe Text)
gofrrsNextPageCount = lens _gofrrsNextPageCount (\ s a -> s{_gofrrsNextPageCount = a})

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
gofrrsNextPageToken :: Lens' GetOperationsForResourceResponse (Maybe Text)
gofrrsNextPageToken = lens _gofrrsNextPageToken (\ s a -> s{_gofrrsNextPageToken = a})

-- | An array of key-value pairs containing information about the results of your get operations for resource request.
gofrrsOperations :: Lens' GetOperationsForResourceResponse [Operation]
gofrrsOperations = lens _gofrrsOperations (\ s a -> s{_gofrrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
gofrrsResponseStatus :: Lens' GetOperationsForResourceResponse Int
gofrrsResponseStatus = lens _gofrrsResponseStatus (\ s a -> s{_gofrrsResponseStatus = a})

instance NFData GetOperationsForResourceResponse
         where
