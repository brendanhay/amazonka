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
    , grchrqChronologicalOrder
    , grchrqNextToken
    , grchrqLimit
    , grchrqLaterTime
    , grchrqEarlierTime
    , grchrqResourceType
    , grchrqResourceId

    -- * Response
    , GetResourceConfigHistoryResponse
    -- ** Response constructor
    , getResourceConfigHistoryResponse
    -- ** Response lenses
    , grchrsNextToken
    , grchrsConfigurationItems
    , grchrsStatus
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
-- * 'grchrqChronologicalOrder'
--
-- * 'grchrqNextToken'
--
-- * 'grchrqLimit'
--
-- * 'grchrqLaterTime'
--
-- * 'grchrqEarlierTime'
--
-- * 'grchrqResourceType'
--
-- * 'grchrqResourceId'
data GetResourceConfigHistory = GetResourceConfigHistory'
    { _grchrqChronologicalOrder :: !(Maybe ChronologicalOrder)
    , _grchrqNextToken          :: !(Maybe Text)
    , _grchrqLimit              :: !(Maybe Nat)
    , _grchrqLaterTime          :: !(Maybe POSIX)
    , _grchrqEarlierTime        :: !(Maybe POSIX)
    , _grchrqResourceType       :: !ResourceType
    , _grchrqResourceId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetResourceConfigHistory' smart constructor.
getResourceConfigHistory :: ResourceType -> Text -> GetResourceConfigHistory
getResourceConfigHistory pResourceType_ pResourceId_ =
    GetResourceConfigHistory'
    { _grchrqChronologicalOrder = Nothing
    , _grchrqNextToken = Nothing
    , _grchrqLimit = Nothing
    , _grchrqLaterTime = Nothing
    , _grchrqEarlierTime = Nothing
    , _grchrqResourceType = pResourceType_
    , _grchrqResourceId = pResourceId_
    }

-- | The chronological order for configuration items listed. By default the
-- results are listed in reverse chronological order.
grchrqChronologicalOrder :: Lens' GetResourceConfigHistory (Maybe ChronologicalOrder)
grchrqChronologicalOrder = lens _grchrqChronologicalOrder (\ s a -> s{_grchrqChronologicalOrder = a});

-- | An optional parameter used for pagination of the results.
grchrqNextToken :: Lens' GetResourceConfigHistory (Maybe Text)
grchrqNextToken = lens _grchrqNextToken (\ s a -> s{_grchrqNextToken = a});

-- | The maximum number of configuration items returned in each page. The
-- default is 10. You cannot specify a limit greater than 100.
grchrqLimit :: Lens' GetResourceConfigHistory (Maybe Natural)
grchrqLimit = lens _grchrqLimit (\ s a -> s{_grchrqLimit = a}) . mapping _Nat;

-- | The time stamp that indicates a later time. If not specified, current
-- time is taken.
grchrqLaterTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchrqLaterTime = lens _grchrqLaterTime (\ s a -> s{_grchrqLaterTime = a}) . mapping _Time;

-- | The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start from when the first configuration item was recorded.
grchrqEarlierTime :: Lens' GetResourceConfigHistory (Maybe UTCTime)
grchrqEarlierTime = lens _grchrqEarlierTime (\ s a -> s{_grchrqEarlierTime = a}) . mapping _Time;

-- | The resource type.
grchrqResourceType :: Lens' GetResourceConfigHistory ResourceType
grchrqResourceType = lens _grchrqResourceType (\ s a -> s{_grchrqResourceType = a});

-- | The ID of the resource (for example., @sg-xxxxxx@).
grchrqResourceId :: Lens' GetResourceConfigHistory Text
grchrqResourceId = lens _grchrqResourceId (\ s a -> s{_grchrqResourceId = a});

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
              ["chronologicalOrder" .= _grchrqChronologicalOrder,
               "nextToken" .= _grchrqNextToken,
               "limit" .= _grchrqLimit,
               "laterTime" .= _grchrqLaterTime,
               "earlierTime" .= _grchrqEarlierTime,
               "resourceType" .= _grchrqResourceType,
               "resourceId" .= _grchrqResourceId]

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
-- * 'grchrsNextToken'
--
-- * 'grchrsConfigurationItems'
--
-- * 'grchrsStatus'
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
    { _grchrsNextToken          :: !(Maybe Text)
    , _grchrsConfigurationItems :: !(Maybe [ConfigurationItem])
    , _grchrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetResourceConfigHistoryResponse' smart constructor.
getResourceConfigHistoryResponse :: Int -> GetResourceConfigHistoryResponse
getResourceConfigHistoryResponse pStatus_ =
    GetResourceConfigHistoryResponse'
    { _grchrsNextToken = Nothing
    , _grchrsConfigurationItems = Nothing
    , _grchrsStatus = pStatus_
    }

-- | A token used for pagination of results.
grchrsNextToken :: Lens' GetResourceConfigHistoryResponse (Maybe Text)
grchrsNextToken = lens _grchrsNextToken (\ s a -> s{_grchrsNextToken = a});

-- | A list that contains the configuration history of one or more resources.
grchrsConfigurationItems :: Lens' GetResourceConfigHistoryResponse [ConfigurationItem]
grchrsConfigurationItems = lens _grchrsConfigurationItems (\ s a -> s{_grchrsConfigurationItems = a}) . _Default;

-- | FIXME: Undocumented member.
grchrsStatus :: Lens' GetResourceConfigHistoryResponse Int
grchrsStatus = lens _grchrsStatus (\ s a -> s{_grchrsStatus = a});
