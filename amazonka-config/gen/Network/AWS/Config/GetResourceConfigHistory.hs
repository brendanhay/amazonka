{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of configuration items for the specified resource. The
-- list contains details about each state of the resource during the
-- specified time interval. You can specify a @limit@ on the number of
-- results returned on the page. If a limit is specified, a @nextToken@ is
-- returned as part of the result that you can use to continue this
-- request.
--
-- Each call to the API is limited to span a duration of seven days. It is
-- likely that the number of records returned is smaller than the specified
-- @limit@. In such cases, you can make another call, using the @nextToken@
-- .
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_GetResourceConfigHistory.html>
module Network.AWS.Config.GetResourceConfigHistory
    (
    -- * Request
      GetResourceConfigHistory
    -- ** Request constructor
    , getResourceConfigHistory
    -- ** Request lenses
    , grchChronologicalOrder
    , grchNextToken
    , grchLimit
    , grchLaterTime
    , grchEarlierTime
    , grchResourceType
    , grchResourceId

    -- * Response
    , GetResourceConfigHistoryResponse
    -- ** Response constructor
    , getResourceConfigHistoryResponse
    -- ** Response lenses
    , grchrNextToken
    , grchrConfigurationItems
    , grchrStatus
    ) where

import           Network.AWS.Config.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the GetResourceConfigHistory action.
--
-- /See:/ 'getResourceConfigHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grchChronologicalOrder'
--
-- * 'grchNextToken'
--
-- * 'grchLimit'
--
-- * 'grchLaterTime'
--
-- * 'grchEarlierTime'
--
-- * 'grchResourceType'
--
-- * 'grchResourceId'
data GetResourceConfigHistory = GetResourceConfigHistory'
    { _grchChronologicalOrder :: !(Maybe ChronologicalOrder)
    , _grchNextToken          :: !(Maybe Text)
    , _grchLimit              :: !(Maybe Nat)
    , _grchLaterTime          :: !(Maybe POSIX)
    , _grchEarlierTime        :: !(Maybe POSIX)
    , _grchResourceType       :: !ResourceType
    , _grchResourceId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetResourceConfigHistory' smart constructor.
getResourceConfigHistory :: ResourceType -> Text -> GetResourceConfigHistory
getResourceConfigHistory pResourceType pResourceId =
    GetResourceConfigHistory'
    { _grchChronologicalOrder = Nothing
    , _grchNextToken = Nothing
    , _grchLimit = Nothing
    , _grchLaterTime = Nothing
    , _grchEarlierTime = Nothing
    , _grchResourceType = pResourceType
    , _grchResourceId = pResourceId
    }

-- | The chronological order for configuration items listed. By default the
-- results are listed in reverse chronological order.
grchChronologicalOrder :: Lens' GetResourceConfigHistory (Maybe ChronologicalOrder)
grchChronologicalOrder = lens _grchChronologicalOrder (\ s a -> s{_grchChronologicalOrder = a});

-- | An optional parameter used for pagination of the results.
grchNextToken :: Lens' GetResourceConfigHistory (Maybe Text)
grchNextToken = lens _grchNextToken (\ s a -> s{_grchNextToken = a});

-- | The maximum number of configuration items returned in each page. The
-- default is 10. You cannot specify a limit greater than 100.
grchLimit :: Lens' GetResourceConfigHistory (Maybe Natural)
grchLimit = lens _grchLimit (\ s a -> s{_grchLimit = a}) . mapping _Nat;

-- | The time stamp that indicates a later time. If not specified, current
-- time is taken.
grchLaterTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchLaterTime = lens _grchLaterTime (\ s a -> s{_grchLaterTime = a}) . mapping _Time;

-- | The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start from when the first configuration item was recorded.
grchEarlierTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchEarlierTime = lens _grchEarlierTime (\ s a -> s{_grchEarlierTime = a}) . mapping _Time;

-- | The resource type.
grchResourceType :: Lens' GetResourceConfigHistory ResourceType
grchResourceType = lens _grchResourceType (\ s a -> s{_grchResourceType = a});

-- | The ID of the resource (for example., @sg-xxxxxx@).
grchResourceId :: Lens' GetResourceConfigHistory Text
grchResourceId = lens _grchResourceId (\ s a -> s{_grchResourceId = a});

instance AWSRequest GetResourceConfigHistory where
        type Sv GetResourceConfigHistory = Config
        type Rs GetResourceConfigHistory =
             GetResourceConfigHistoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetResourceConfigHistoryResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "configurationItems" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["chronologicalOrder" .= _grchChronologicalOrder,
               "nextToken" .= _grchNextToken, "limit" .= _grchLimit,
               "laterTime" .= _grchLaterTime,
               "earlierTime" .= _grchEarlierTime,
               "resourceType" .= _grchResourceType,
               "resourceId" .= _grchResourceId]

instance ToPath GetResourceConfigHistory where
        toPath = const "/"

instance ToQuery GetResourceConfigHistory where
        toQuery = const mempty

-- | The output for the GetResourceConfigHistory action.
--
-- /See:/ 'getResourceConfigHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grchrNextToken'
--
-- * 'grchrConfigurationItems'
--
-- * 'grchrStatus'
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
    { _grchrNextToken          :: !(Maybe Text)
    , _grchrConfigurationItems :: !(Maybe [ConfigurationItem])
    , _grchrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetResourceConfigHistoryResponse' smart constructor.
getResourceConfigHistoryResponse :: Int -> GetResourceConfigHistoryResponse
getResourceConfigHistoryResponse pStatus =
    GetResourceConfigHistoryResponse'
    { _grchrNextToken = Nothing
    , _grchrConfigurationItems = Nothing
    , _grchrStatus = pStatus
    }

-- | A token used for pagination of results.
grchrNextToken :: Lens' GetResourceConfigHistoryResponse (Maybe Text)
grchrNextToken = lens _grchrNextToken (\ s a -> s{_grchrNextToken = a});

-- | A list that contains the configuration history of one or more resources.
grchrConfigurationItems :: Lens' GetResourceConfigHistoryResponse [ConfigurationItem]
grchrConfigurationItems = lens _grchrConfigurationItems (\ s a -> s{_grchrConfigurationItems = a}) . _Default;

-- | FIXME: Undocumented member.
grchrStatus :: Lens' GetResourceConfigHistoryResponse Int
grchrStatus = lens _grchrStatus (\ s a -> s{_grchrStatus = a});
