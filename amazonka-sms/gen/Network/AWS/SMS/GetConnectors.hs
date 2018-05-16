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
-- Module      : Network.AWS.SMS.GetConnectors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetConnectors API returns a list of connectors that are registered with the Server Migration Service.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetConnectors
    (
    -- * Creating a Request
      getConnectors
    , GetConnectors
    -- * Request Lenses
    , gcNextToken
    , gcMaxResults

    -- * Destructuring the Response
    , getConnectorsResponse
    , GetConnectorsResponse
    -- * Response Lenses
    , gcrsConnectorList
    , gcrsNextToken
    , gcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getConnectors' smart constructor.
data GetConnectors = GetConnectors'
  { _gcNextToken  :: !(Maybe Text)
  , _gcMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConnectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcNextToken' - Undocumented member.
--
-- * 'gcMaxResults' - Undocumented member.
getConnectors
    :: GetConnectors
getConnectors = GetConnectors' {_gcNextToken = Nothing, _gcMaxResults = Nothing}


-- | Undocumented member.
gcNextToken :: Lens' GetConnectors (Maybe Text)
gcNextToken = lens _gcNextToken (\ s a -> s{_gcNextToken = a})

-- | Undocumented member.
gcMaxResults :: Lens' GetConnectors (Maybe Int)
gcMaxResults = lens _gcMaxResults (\ s a -> s{_gcMaxResults = a})

instance AWSPager GetConnectors where
        page rq rs
          | stop (rs ^. gcrsNextToken) = Nothing
          | stop (rs ^. gcrsConnectorList) = Nothing
          | otherwise =
            Just $ rq & gcNextToken .~ rs ^. gcrsNextToken

instance AWSRequest GetConnectors where
        type Rs GetConnectors = GetConnectorsResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetConnectorsResponse' <$>
                   (x .?> "connectorList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetConnectors where

instance NFData GetConnectors where

instance ToHeaders GetConnectors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetConnectors"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetConnectors where
        toJSON GetConnectors'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _gcNextToken,
                  ("maxResults" .=) <$> _gcMaxResults])

instance ToPath GetConnectors where
        toPath = const "/"

instance ToQuery GetConnectors where
        toQuery = const mempty

-- | /See:/ 'getConnectorsResponse' smart constructor.
data GetConnectorsResponse = GetConnectorsResponse'
  { _gcrsConnectorList  :: !(Maybe [Connector])
  , _gcrsNextToken      :: !(Maybe Text)
  , _gcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConnectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsConnectorList' - Undocumented member.
--
-- * 'gcrsNextToken' - Undocumented member.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getConnectorsResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> GetConnectorsResponse
getConnectorsResponse pResponseStatus_ =
  GetConnectorsResponse'
    { _gcrsConnectorList = Nothing
    , _gcrsNextToken = Nothing
    , _gcrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
gcrsConnectorList :: Lens' GetConnectorsResponse [Connector]
gcrsConnectorList = lens _gcrsConnectorList (\ s a -> s{_gcrsConnectorList = a}) . _Default . _Coerce

-- | Undocumented member.
gcrsNextToken :: Lens' GetConnectorsResponse (Maybe Text)
gcrsNextToken = lens _gcrsNextToken (\ s a -> s{_gcrsNextToken = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetConnectorsResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetConnectorsResponse where
