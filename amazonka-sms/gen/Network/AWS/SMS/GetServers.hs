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
-- Module      : Network.AWS.SMS.GetServers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetServers API returns a list of all servers in your server catalog. For this call to succeed, you must previously have called ImportServerCatalog.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetServers
    (
    -- * Creating a Request
      getServers
    , GetServers
    -- * Request Lenses
    , gsNextToken
    , gsMaxResults

    -- * Destructuring the Response
    , getServersResponse
    , GetServersResponse
    -- * Response Lenses
    , gsrsServerCatalogStatus
    , gsrsLastModifiedOn
    , gsrsNextToken
    , gsrsServerList
    , gsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getServers' smart constructor.
data GetServers = GetServers'
  { _gsNextToken  :: !(Maybe Text)
  , _gsMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsNextToken' - Undocumented member.
--
-- * 'gsMaxResults' - Undocumented member.
getServers
    :: GetServers
getServers = GetServers' {_gsNextToken = Nothing, _gsMaxResults = Nothing}


-- | Undocumented member.
gsNextToken :: Lens' GetServers (Maybe Text)
gsNextToken = lens _gsNextToken (\ s a -> s{_gsNextToken = a})

-- | Undocumented member.
gsMaxResults :: Lens' GetServers (Maybe Int)
gsMaxResults = lens _gsMaxResults (\ s a -> s{_gsMaxResults = a})

instance AWSPager GetServers where
        page rq rs
          | stop (rs ^. gsrsNextToken) = Nothing
          | stop (rs ^. gsrsServerList) = Nothing
          | otherwise =
            Just $ rq & gsNextToken .~ rs ^. gsrsNextToken

instance AWSRequest GetServers where
        type Rs GetServers = GetServersResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetServersResponse' <$>
                   (x .?> "serverCatalogStatus") <*>
                     (x .?> "lastModifiedOn")
                     <*> (x .?> "nextToken")
                     <*> (x .?> "serverList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetServers where

instance NFData GetServers where

instance ToHeaders GetServers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetServers"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetServers where
        toJSON GetServers'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _gsNextToken,
                  ("maxResults" .=) <$> _gsMaxResults])

instance ToPath GetServers where
        toPath = const "/"

instance ToQuery GetServers where
        toQuery = const mempty

-- | /See:/ 'getServersResponse' smart constructor.
data GetServersResponse = GetServersResponse'
  { _gsrsServerCatalogStatus :: !(Maybe ServerCatalogStatus)
  , _gsrsLastModifiedOn      :: !(Maybe POSIX)
  , _gsrsNextToken           :: !(Maybe Text)
  , _gsrsServerList          :: !(Maybe [Server])
  , _gsrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrsServerCatalogStatus' - Undocumented member.
--
-- * 'gsrsLastModifiedOn' - Undocumented member.
--
-- * 'gsrsNextToken' - Undocumented member.
--
-- * 'gsrsServerList' - Undocumented member.
--
-- * 'gsrsResponseStatus' - -- | The response status code.
getServersResponse
    :: Int -- ^ 'gsrsResponseStatus'
    -> GetServersResponse
getServersResponse pResponseStatus_ =
  GetServersResponse'
    { _gsrsServerCatalogStatus = Nothing
    , _gsrsLastModifiedOn = Nothing
    , _gsrsNextToken = Nothing
    , _gsrsServerList = Nothing
    , _gsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gsrsServerCatalogStatus :: Lens' GetServersResponse (Maybe ServerCatalogStatus)
gsrsServerCatalogStatus = lens _gsrsServerCatalogStatus (\ s a -> s{_gsrsServerCatalogStatus = a})

-- | Undocumented member.
gsrsLastModifiedOn :: Lens' GetServersResponse (Maybe UTCTime)
gsrsLastModifiedOn = lens _gsrsLastModifiedOn (\ s a -> s{_gsrsLastModifiedOn = a}) . mapping _Time

-- | Undocumented member.
gsrsNextToken :: Lens' GetServersResponse (Maybe Text)
gsrsNextToken = lens _gsrsNextToken (\ s a -> s{_gsrsNextToken = a})

-- | Undocumented member.
gsrsServerList :: Lens' GetServersResponse [Server]
gsrsServerList = lens _gsrsServerList (\ s a -> s{_gsrsServerList = a}) . _Default . _Coerce

-- | -- | The response status code.
gsrsResponseStatus :: Lens' GetServersResponse Int
gsrsResponseStatus = lens _gsrsResponseStatus (\ s a -> s{_gsrsResponseStatus = a})

instance NFData GetServersResponse where
