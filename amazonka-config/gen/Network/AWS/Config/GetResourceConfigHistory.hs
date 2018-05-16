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
-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval.
--
--
-- The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
--
-- This operation returns paginated results.
module Network.AWS.Config.GetResourceConfigHistory
    (
    -- * Creating a Request
      getResourceConfigHistory
    , GetResourceConfigHistory
    -- * Request Lenses
    , grchChronologicalOrder
    , grchNextToken
    , grchLimit
    , grchLaterTime
    , grchEarlierTime
    , grchResourceType
    , grchResourceId

    -- * Destructuring the Response
    , getResourceConfigHistoryResponse
    , GetResourceConfigHistoryResponse
    -- * Response Lenses
    , grchrsNextToken
    , grchrsConfigurationItems
    , grchrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'GetResourceConfigHistory' action.
--
--
--
-- /See:/ 'getResourceConfigHistory' smart constructor.
data GetResourceConfigHistory = GetResourceConfigHistory'
  { _grchChronologicalOrder :: !(Maybe ChronologicalOrder)
  , _grchNextToken          :: !(Maybe Text)
  , _grchLimit              :: !(Maybe Nat)
  , _grchLaterTime          :: !(Maybe POSIX)
  , _grchEarlierTime        :: !(Maybe POSIX)
  , _grchResourceType       :: !ResourceType
  , _grchResourceId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceConfigHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grchChronologicalOrder' - The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
--
-- * 'grchNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'grchLimit' - The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- * 'grchLaterTime' - The time stamp that indicates a later time. If not specified, current time is taken.
--
-- * 'grchEarlierTime' - The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
--
-- * 'grchResourceType' - The resource type.
--
-- * 'grchResourceId' - The ID of the resource (for example., @sg-xxxxxx@ ).
getResourceConfigHistory
    :: ResourceType -- ^ 'grchResourceType'
    -> Text -- ^ 'grchResourceId'
    -> GetResourceConfigHistory
getResourceConfigHistory pResourceType_ pResourceId_ =
  GetResourceConfigHistory'
    { _grchChronologicalOrder = Nothing
    , _grchNextToken = Nothing
    , _grchLimit = Nothing
    , _grchLaterTime = Nothing
    , _grchEarlierTime = Nothing
    , _grchResourceType = pResourceType_
    , _grchResourceId = pResourceId_
    }


-- | The chronological order for configuration items listed. By default, the results are listed in reverse chronological order.
grchChronologicalOrder :: Lens' GetResourceConfigHistory (Maybe ChronologicalOrder)
grchChronologicalOrder = lens _grchChronologicalOrder (\ s a -> s{_grchChronologicalOrder = a})

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
grchNextToken :: Lens' GetResourceConfigHistory (Maybe Text)
grchNextToken = lens _grchNextToken (\ s a -> s{_grchNextToken = a})

-- | The maximum number of configuration items returned on each page. The default is 10. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
grchLimit :: Lens' GetResourceConfigHistory (Maybe Natural)
grchLimit = lens _grchLimit (\ s a -> s{_grchLimit = a}) . mapping _Nat

-- | The time stamp that indicates a later time. If not specified, current time is taken.
grchLaterTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchLaterTime = lens _grchLaterTime (\ s a -> s{_grchLaterTime = a}) . mapping _Time

-- | The time stamp that indicates an earlier time. If not specified, the action returns paginated results that contain configuration items that start when the first configuration item was recorded.
grchEarlierTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchEarlierTime = lens _grchEarlierTime (\ s a -> s{_grchEarlierTime = a}) . mapping _Time

-- | The resource type.
grchResourceType :: Lens' GetResourceConfigHistory ResourceType
grchResourceType = lens _grchResourceType (\ s a -> s{_grchResourceType = a})

-- | The ID of the resource (for example., @sg-xxxxxx@ ).
grchResourceId :: Lens' GetResourceConfigHistory Text
grchResourceId = lens _grchResourceId (\ s a -> s{_grchResourceId = a})

instance AWSPager GetResourceConfigHistory where
        page rq rs
          | stop (rs ^. grchrsNextToken) = Nothing
          | stop (rs ^. grchrsConfigurationItems) = Nothing
          | otherwise =
            Just $ rq & grchNextToken .~ rs ^. grchrsNextToken

instance AWSRequest GetResourceConfigHistory where
        type Rs GetResourceConfigHistory =
             GetResourceConfigHistoryResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetResourceConfigHistoryResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "configurationItems" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetResourceConfigHistory where

instance NFData GetResourceConfigHistory where

instance ToHeaders GetResourceConfigHistory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetResourceConfigHistory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetResourceConfigHistory where
        toJSON GetResourceConfigHistory'{..}
          = object
              (catMaybes
                 [("chronologicalOrder" .=) <$>
                    _grchChronologicalOrder,
                  ("nextToken" .=) <$> _grchNextToken,
                  ("limit" .=) <$> _grchLimit,
                  ("laterTime" .=) <$> _grchLaterTime,
                  ("earlierTime" .=) <$> _grchEarlierTime,
                  Just ("resourceType" .= _grchResourceType),
                  Just ("resourceId" .= _grchResourceId)])

instance ToPath GetResourceConfigHistory where
        toPath = const "/"

instance ToQuery GetResourceConfigHistory where
        toQuery = const mempty

-- | The output for the 'GetResourceConfigHistory' action.
--
--
--
-- /See:/ 'getResourceConfigHistoryResponse' smart constructor.
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
  { _grchrsNextToken          :: !(Maybe Text)
  , _grchrsConfigurationItems :: !(Maybe [ConfigurationItem])
  , _grchrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceConfigHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grchrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'grchrsConfigurationItems' - A list that contains the configuration history of one or more resources.
--
-- * 'grchrsResponseStatus' - -- | The response status code.
getResourceConfigHistoryResponse
    :: Int -- ^ 'grchrsResponseStatus'
    -> GetResourceConfigHistoryResponse
getResourceConfigHistoryResponse pResponseStatus_ =
  GetResourceConfigHistoryResponse'
    { _grchrsNextToken = Nothing
    , _grchrsConfigurationItems = Nothing
    , _grchrsResponseStatus = pResponseStatus_
    }


-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
grchrsNextToken :: Lens' GetResourceConfigHistoryResponse (Maybe Text)
grchrsNextToken = lens _grchrsNextToken (\ s a -> s{_grchrsNextToken = a})

-- | A list that contains the configuration history of one or more resources.
grchrsConfigurationItems :: Lens' GetResourceConfigHistoryResponse [ConfigurationItem]
grchrsConfigurationItems = lens _grchrsConfigurationItems (\ s a -> s{_grchrsConfigurationItems = a}) . _Default . _Coerce

-- | -- | The response status code.
grchrsResponseStatus :: Lens' GetResourceConfigHistoryResponse Int
grchrsResponseStatus = lens _grchrsResponseStatus (\ s a -> s{_grchrsResponseStatus = a})

instance NFData GetResourceConfigHistoryResponse
         where
