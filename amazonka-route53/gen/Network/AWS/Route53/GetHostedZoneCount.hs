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
-- Module      : Network.AWS.Route53.GetHostedZoneCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of hosted zones that are associated with the current AWS account.
--
--
module Network.AWS.Route53.GetHostedZoneCount
    (
    -- * Creating a Request
      getHostedZoneCount
    , GetHostedZoneCount

    -- * Destructuring the Response
    , getHostedZoneCountResponse
    , GetHostedZoneCountResponse
    -- * Response Lenses
    , ghzcrsResponseStatus
    , ghzcrsHostedZoneCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to retrieve a count of all the hosted zones that are associated with the current AWS account.
--
--
--
-- /See:/ 'getHostedZoneCount' smart constructor.
data GetHostedZoneCount =
  GetHostedZoneCount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostedZoneCount' with the minimum fields required to make a request.
--
getHostedZoneCount
    :: GetHostedZoneCount
getHostedZoneCount = GetHostedZoneCount'


instance AWSRequest GetHostedZoneCount where
        type Rs GetHostedZoneCount =
             GetHostedZoneCountResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHostedZoneCountResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HostedZoneCount"))

instance Hashable GetHostedZoneCount where

instance NFData GetHostedZoneCount where

instance ToHeaders GetHostedZoneCount where
        toHeaders = const mempty

instance ToPath GetHostedZoneCount where
        toPath = const "/2013-04-01/hostedzonecount"

instance ToQuery GetHostedZoneCount where
        toQuery = const mempty

-- | A complex type that contains the response to a @GetHostedZoneCount@ request.
--
--
--
-- /See:/ 'getHostedZoneCountResponse' smart constructor.
data GetHostedZoneCountResponse = GetHostedZoneCountResponse'
  { _ghzcrsResponseStatus  :: !Int
  , _ghzcrsHostedZoneCount :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostedZoneCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghzcrsResponseStatus' - -- | The response status code.
--
-- * 'ghzcrsHostedZoneCount' - The total number of public and private hosted zones that are associated with the current AWS account.
getHostedZoneCountResponse
    :: Int -- ^ 'ghzcrsResponseStatus'
    -> Integer -- ^ 'ghzcrsHostedZoneCount'
    -> GetHostedZoneCountResponse
getHostedZoneCountResponse pResponseStatus_ pHostedZoneCount_ =
  GetHostedZoneCountResponse'
    { _ghzcrsResponseStatus = pResponseStatus_
    , _ghzcrsHostedZoneCount = pHostedZoneCount_
    }


-- | -- | The response status code.
ghzcrsResponseStatus :: Lens' GetHostedZoneCountResponse Int
ghzcrsResponseStatus = lens _ghzcrsResponseStatus (\ s a -> s{_ghzcrsResponseStatus = a})

-- | The total number of public and private hosted zones that are associated with the current AWS account.
ghzcrsHostedZoneCount :: Lens' GetHostedZoneCountResponse Integer
ghzcrsHostedZoneCount = lens _ghzcrsHostedZoneCount (\ s a -> s{_ghzcrsHostedZoneCount = a})

instance NFData GetHostedZoneCountResponse where
