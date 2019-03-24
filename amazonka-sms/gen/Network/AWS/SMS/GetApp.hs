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
-- Module      : Network.AWS.SMS.GetApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about an application.
--
--
module Network.AWS.SMS.GetApp
    (
    -- * Creating a Request
      getApp
    , GetApp
    -- * Request Lenses
    , gaAppId

    -- * Destructuring the Response
    , getAppResponse
    , GetAppResponse
    -- * Response Lenses
    , garsAppSummary
    , garsServerGroups
    , garsTags
    , garsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getApp' smart constructor.
newtype GetApp = GetApp'
  { _gaAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaAppId' - ID of the application whose information is being retrieved.
getApp
    :: GetApp
getApp = GetApp' {_gaAppId = Nothing}


-- | ID of the application whose information is being retrieved.
gaAppId :: Lens' GetApp (Maybe Text)
gaAppId = lens _gaAppId (\ s a -> s{_gaAppId = a})

instance AWSRequest GetApp where
        type Rs GetApp = GetAppResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetAppResponse' <$>
                   (x .?> "appSummary") <*>
                     (x .?> "serverGroups" .!@ mempty)
                     <*> (x .?> "tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetApp where

instance NFData GetApp where

instance ToHeaders GetApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetApp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetApp where
        toJSON GetApp'{..}
          = object (catMaybes [("appId" .=) <$> _gaAppId])

instance ToPath GetApp where
        toPath = const "/"

instance ToQuery GetApp where
        toQuery = const mempty

-- | /See:/ 'getAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { _garsAppSummary     :: !(Maybe AppSummary)
  , _garsServerGroups   :: !(Maybe [ServerGroup])
  , _garsTags           :: !(Maybe [Tag])
  , _garsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garsAppSummary' - Information about the application.
--
-- * 'garsServerGroups' - List of server groups belonging to the application.
--
-- * 'garsTags' - List of tags associated with the application.
--
-- * 'garsResponseStatus' - -- | The response status code.
getAppResponse
    :: Int -- ^ 'garsResponseStatus'
    -> GetAppResponse
getAppResponse pResponseStatus_ =
  GetAppResponse'
    { _garsAppSummary = Nothing
    , _garsServerGroups = Nothing
    , _garsTags = Nothing
    , _garsResponseStatus = pResponseStatus_
    }


-- | Information about the application.
garsAppSummary :: Lens' GetAppResponse (Maybe AppSummary)
garsAppSummary = lens _garsAppSummary (\ s a -> s{_garsAppSummary = a})

-- | List of server groups belonging to the application.
garsServerGroups :: Lens' GetAppResponse [ServerGroup]
garsServerGroups = lens _garsServerGroups (\ s a -> s{_garsServerGroups = a}) . _Default . _Coerce

-- | List of tags associated with the application.
garsTags :: Lens' GetAppResponse [Tag]
garsTags = lens _garsTags (\ s a -> s{_garsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
garsResponseStatus :: Lens' GetAppResponse Int
garsResponseStatus = lens _garsResponseStatus (\ s a -> s{_garsResponseStatus = a})

instance NFData GetAppResponse where
