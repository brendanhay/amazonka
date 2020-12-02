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
-- Module      : Network.AWS.Discovery.GetDiscoverySummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a short summary of discovered assets.
--
--
module Network.AWS.Discovery.GetDiscoverySummary
    (
    -- * Creating a Request
      getDiscoverySummary
    , GetDiscoverySummary

    -- * Destructuring the Response
    , getDiscoverySummaryResponse
    , GetDiscoverySummaryResponse
    -- * Response Lenses
    , gdsrsServers
    , gdsrsServersMappedtoTags
    , gdsrsServersMappedToApplications
    , gdsrsConnectorSummary
    , gdsrsAgentSummary
    , gdsrsApplications
    , gdsrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDiscoverySummary' smart constructor.
data GetDiscoverySummary =
  GetDiscoverySummary'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiscoverySummary' with the minimum fields required to make a request.
--
getDiscoverySummary
    :: GetDiscoverySummary
getDiscoverySummary = GetDiscoverySummary'


instance AWSRequest GetDiscoverySummary where
        type Rs GetDiscoverySummary =
             GetDiscoverySummaryResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 GetDiscoverySummaryResponse' <$>
                   (x .?> "servers") <*> (x .?> "serversMappedtoTags")
                     <*> (x .?> "serversMappedToApplications")
                     <*> (x .?> "connectorSummary")
                     <*> (x .?> "agentSummary")
                     <*> (x .?> "applications")
                     <*> (pure (fromEnum s)))

instance Hashable GetDiscoverySummary where

instance NFData GetDiscoverySummary where

instance ToHeaders GetDiscoverySummary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.GetDiscoverySummary"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDiscoverySummary where
        toJSON = const (Object mempty)

instance ToPath GetDiscoverySummary where
        toPath = const "/"

instance ToQuery GetDiscoverySummary where
        toQuery = const mempty

-- | /See:/ 'getDiscoverySummaryResponse' smart constructor.
data GetDiscoverySummaryResponse = GetDiscoverySummaryResponse'
  { _gdsrsServers                     :: !(Maybe Integer)
  , _gdsrsServersMappedtoTags         :: !(Maybe Integer)
  , _gdsrsServersMappedToApplications :: !(Maybe Integer)
  , _gdsrsConnectorSummary            :: !(Maybe CustomerConnectorInfo)
  , _gdsrsAgentSummary                :: !(Maybe CustomerAgentInfo)
  , _gdsrsApplications                :: !(Maybe Integer)
  , _gdsrsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDiscoverySummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsServers' - The number of servers discovered.
--
-- * 'gdsrsServersMappedtoTags' - The number of servers mapped to tags.
--
-- * 'gdsrsServersMappedToApplications' - The number of servers mapped to applications.
--
-- * 'gdsrsConnectorSummary' - Details about discovered connectors, including connector status and health.
--
-- * 'gdsrsAgentSummary' - Details about discovered agents, including agent status and health.
--
-- * 'gdsrsApplications' - The number of applications discovered.
--
-- * 'gdsrsResponseStatus' - -- | The response status code.
getDiscoverySummaryResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GetDiscoverySummaryResponse
getDiscoverySummaryResponse pResponseStatus_ =
  GetDiscoverySummaryResponse'
    { _gdsrsServers = Nothing
    , _gdsrsServersMappedtoTags = Nothing
    , _gdsrsServersMappedToApplications = Nothing
    , _gdsrsConnectorSummary = Nothing
    , _gdsrsAgentSummary = Nothing
    , _gdsrsApplications = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    }


-- | The number of servers discovered.
gdsrsServers :: Lens' GetDiscoverySummaryResponse (Maybe Integer)
gdsrsServers = lens _gdsrsServers (\ s a -> s{_gdsrsServers = a})

-- | The number of servers mapped to tags.
gdsrsServersMappedtoTags :: Lens' GetDiscoverySummaryResponse (Maybe Integer)
gdsrsServersMappedtoTags = lens _gdsrsServersMappedtoTags (\ s a -> s{_gdsrsServersMappedtoTags = a})

-- | The number of servers mapped to applications.
gdsrsServersMappedToApplications :: Lens' GetDiscoverySummaryResponse (Maybe Integer)
gdsrsServersMappedToApplications = lens _gdsrsServersMappedToApplications (\ s a -> s{_gdsrsServersMappedToApplications = a})

-- | Details about discovered connectors, including connector status and health.
gdsrsConnectorSummary :: Lens' GetDiscoverySummaryResponse (Maybe CustomerConnectorInfo)
gdsrsConnectorSummary = lens _gdsrsConnectorSummary (\ s a -> s{_gdsrsConnectorSummary = a})

-- | Details about discovered agents, including agent status and health.
gdsrsAgentSummary :: Lens' GetDiscoverySummaryResponse (Maybe CustomerAgentInfo)
gdsrsAgentSummary = lens _gdsrsAgentSummary (\ s a -> s{_gdsrsAgentSummary = a})

-- | The number of applications discovered.
gdsrsApplications :: Lens' GetDiscoverySummaryResponse (Maybe Integer)
gdsrsApplications = lens _gdsrsApplications (\ s a -> s{_gdsrsApplications = a})

-- | -- | The response status code.
gdsrsResponseStatus :: Lens' GetDiscoverySummaryResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a})

instance NFData GetDiscoverySummaryResponse where
