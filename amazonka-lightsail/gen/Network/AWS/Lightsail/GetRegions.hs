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
-- Module      : Network.AWS.Lightsail.GetRegions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all valid regions for Amazon Lightsail. Use the @include availability zones@ parameter to also return the availability zones in a region.
--
--
module Network.AWS.Lightsail.GetRegions
    (
    -- * Creating a Request
      getRegions
    , GetRegions
    -- * Request Lenses
    , grIncludeAvailabilityZones

    -- * Destructuring the Response
    , getRegionsResponse
    , GetRegionsResponse
    -- * Response Lenses
    , grrsRegions
    , grrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRegions' smart constructor.
newtype GetRegions = GetRegions'
  { _grIncludeAvailabilityZones :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grIncludeAvailabilityZones' - A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
getRegions
    :: GetRegions
getRegions = GetRegions' {_grIncludeAvailabilityZones = Nothing}


-- | A Boolean value indicating whether to also include Availability Zones in your get regions request. Availability Zones are indicated with a letter: e.g., @us-east-2a@ .
grIncludeAvailabilityZones :: Lens' GetRegions (Maybe Bool)
grIncludeAvailabilityZones = lens _grIncludeAvailabilityZones (\ s a -> s{_grIncludeAvailabilityZones = a})

instance AWSRequest GetRegions where
        type Rs GetRegions = GetRegionsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRegionsResponse' <$>
                   (x .?> "regions" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable GetRegions where

instance NFData GetRegions where

instance ToHeaders GetRegions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRegions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRegions where
        toJSON GetRegions'{..}
          = object
              (catMaybes
                 [("includeAvailabilityZones" .=) <$>
                    _grIncludeAvailabilityZones])

instance ToPath GetRegions where
        toPath = const "/"

instance ToQuery GetRegions where
        toQuery = const mempty

-- | /See:/ 'getRegionsResponse' smart constructor.
data GetRegionsResponse = GetRegionsResponse'
  { _grrsRegions        :: !(Maybe [RegionInfo])
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRegionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRegions' - An array of key-value pairs containing information about your get regions request.
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRegionsResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRegionsResponse
getRegionsResponse pResponseStatus_ =
  GetRegionsResponse'
    {_grrsRegions = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about your get regions request.
grrsRegions :: Lens' GetRegionsResponse [RegionInfo]
grrsRegions = lens _grrsRegions (\ s a -> s{_grrsRegions = a}) . _Default . _Coerce

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRegionsResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRegionsResponse where
