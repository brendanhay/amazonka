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
-- Module      : Network.AWS.EMR.ListSecurityConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the security configurations visible to this account, providing their creation dates and times, and their names. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListSecurityConfigurations calls.
--
--
module Network.AWS.EMR.ListSecurityConfigurations
    (
    -- * Creating a Request
      listSecurityConfigurations
    , ListSecurityConfigurations
    -- * Request Lenses
    , lscMarker

    -- * Destructuring the Response
    , listSecurityConfigurationsResponse
    , ListSecurityConfigurationsResponse
    -- * Response Lenses
    , lscrsSecurityConfigurations
    , lscrsMarker
    , lscrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSecurityConfigurations' smart constructor.
newtype ListSecurityConfigurations = ListSecurityConfigurations'
  { _lscMarker :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecurityConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscMarker' - The pagination token that indicates the set of results to retrieve.
listSecurityConfigurations
    :: ListSecurityConfigurations
listSecurityConfigurations = ListSecurityConfigurations' {_lscMarker = Nothing}


-- | The pagination token that indicates the set of results to retrieve.
lscMarker :: Lens' ListSecurityConfigurations (Maybe Text)
lscMarker = lens _lscMarker (\ s a -> s{_lscMarker = a})

instance AWSRequest ListSecurityConfigurations where
        type Rs ListSecurityConfigurations =
             ListSecurityConfigurationsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListSecurityConfigurationsResponse' <$>
                   (x .?> "SecurityConfigurations" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable ListSecurityConfigurations where

instance NFData ListSecurityConfigurations where

instance ToHeaders ListSecurityConfigurations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListSecurityConfigurations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSecurityConfigurations where
        toJSON ListSecurityConfigurations'{..}
          = object (catMaybes [("Marker" .=) <$> _lscMarker])

instance ToPath ListSecurityConfigurations where
        toPath = const "/"

instance ToQuery ListSecurityConfigurations where
        toQuery = const mempty

-- | /See:/ 'listSecurityConfigurationsResponse' smart constructor.
data ListSecurityConfigurationsResponse = ListSecurityConfigurationsResponse'
  { _lscrsSecurityConfigurations :: !(Maybe [SecurityConfigurationSummary])
  , _lscrsMarker                 :: !(Maybe Text)
  , _lscrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecurityConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscrsSecurityConfigurations' - The creation date and time, and name, of each security configuration.
--
-- * 'lscrsMarker' - A pagination token that indicates the next set of results to retrieve. Include the marker in the next ListSecurityConfiguration call to retrieve the next page of results, if required.
--
-- * 'lscrsResponseStatus' - -- | The response status code.
listSecurityConfigurationsResponse
    :: Int -- ^ 'lscrsResponseStatus'
    -> ListSecurityConfigurationsResponse
listSecurityConfigurationsResponse pResponseStatus_ =
  ListSecurityConfigurationsResponse'
    { _lscrsSecurityConfigurations = Nothing
    , _lscrsMarker = Nothing
    , _lscrsResponseStatus = pResponseStatus_
    }


-- | The creation date and time, and name, of each security configuration.
lscrsSecurityConfigurations :: Lens' ListSecurityConfigurationsResponse [SecurityConfigurationSummary]
lscrsSecurityConfigurations = lens _lscrsSecurityConfigurations (\ s a -> s{_lscrsSecurityConfigurations = a}) . _Default . _Coerce

-- | A pagination token that indicates the next set of results to retrieve. Include the marker in the next ListSecurityConfiguration call to retrieve the next page of results, if required.
lscrsMarker :: Lens' ListSecurityConfigurationsResponse (Maybe Text)
lscrsMarker = lens _lscrsMarker (\ s a -> s{_lscrsMarker = a})

-- | -- | The response status code.
lscrsResponseStatus :: Lens' ListSecurityConfigurationsResponse Int
lscrsResponseStatus = lens _lscrsResponseStatus (\ s a -> s{_lscrsResponseStatus = a})

instance NFData ListSecurityConfigurationsResponse
         where
