{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZoneCount
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a count of all your hosted zones, send a @GET@ request to
-- the @2013-04-01\/hostedzonecount@ resource.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZoneCount.html AWS API Reference> for GetHostedZoneCount.
module Network.AWS.Route53.GetHostedZoneCount
    (
    -- * Creating a Request
      GetHostedZoneCount
    , getHostedZoneCount

    -- * Destructuring the Response
    , GetHostedZoneCountResponse
    , getHostedZoneCountResponse
    -- * Response Lenses
    , ghzcrsStatus
    , ghzcrsHostedZoneCount
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | To retrieve a count of all your hosted zones, send a @GET@ request to
-- the @2013-04-01\/hostedzonecount@ resource.
--
-- /See:/ 'getHostedZoneCount' smart constructor.
data GetHostedZoneCount =
    GetHostedZoneCount'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHostedZoneCount' smart constructor.
getHostedZoneCount :: GetHostedZoneCount
getHostedZoneCount = GetHostedZoneCount'

instance AWSRequest GetHostedZoneCount where
        type Sv GetHostedZoneCount = Route53
        type Rs GetHostedZoneCount =
             GetHostedZoneCountResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetHostedZoneCountResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HostedZoneCount"))

instance ToHeaders GetHostedZoneCount where
        toHeaders = const mempty

instance ToPath GetHostedZoneCount where
        toPath = const "/2013-04-01/hostedzonecount"

instance ToQuery GetHostedZoneCount where
        toQuery = const mempty

-- | A complex type that contains the count of hosted zones associated with
-- the current AWS account.
--
-- /See:/ 'getHostedZoneCountResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghzcrsStatus'
--
-- * 'ghzcrsHostedZoneCount'
data GetHostedZoneCountResponse = GetHostedZoneCountResponse'
    { _ghzcrsStatus          :: !Int
    , _ghzcrsHostedZoneCount :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetHostedZoneCountResponse' smart constructor.
getHostedZoneCountResponse :: Int -> Integer -> GetHostedZoneCountResponse
getHostedZoneCountResponse pStatus_ pHostedZoneCount_ =
    GetHostedZoneCountResponse'
    { _ghzcrsStatus = pStatus_
    , _ghzcrsHostedZoneCount = pHostedZoneCount_
    }

-- | Undocumented member.
ghzcrsStatus :: Lens' GetHostedZoneCountResponse Int
ghzcrsStatus = lens _ghzcrsStatus (\ s a -> s{_ghzcrsStatus = a});

-- | The number of hosted zones associated with the current AWS account.
ghzcrsHostedZoneCount :: Lens' GetHostedZoneCountResponse Integer
ghzcrsHostedZoneCount = lens _ghzcrsHostedZoneCount (\ s a -> s{_ghzcrsHostedZoneCount = a});
