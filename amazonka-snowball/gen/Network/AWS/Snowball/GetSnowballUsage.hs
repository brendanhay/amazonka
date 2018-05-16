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
-- Module      : Network.AWS.Snowball.GetSnowballUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Snowball service limit for your account, and also the number of Snowballs your account has in use.
--
--
-- The default service limit for the number of Snowballs that you can have at one time is 1. If you want to increase your service limit, contact AWS Support.
--
module Network.AWS.Snowball.GetSnowballUsage
    (
    -- * Creating a Request
      getSnowballUsage
    , GetSnowballUsage

    -- * Destructuring the Response
    , getSnowballUsageResponse
    , GetSnowballUsageResponse
    -- * Response Lenses
    , gsursSnowballsInUse
    , gsursSnowballLimit
    , gsursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'getSnowballUsage' smart constructor.
data GetSnowballUsage =
  GetSnowballUsage'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSnowballUsage' with the minimum fields required to make a request.
--
getSnowballUsage
    :: GetSnowballUsage
getSnowballUsage = GetSnowballUsage'


instance AWSRequest GetSnowballUsage where
        type Rs GetSnowballUsage = GetSnowballUsageResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 GetSnowballUsageResponse' <$>
                   (x .?> "SnowballsInUse") <*> (x .?> "SnowballLimit")
                     <*> (pure (fromEnum s)))

instance Hashable GetSnowballUsage where

instance NFData GetSnowballUsage where

instance ToHeaders GetSnowballUsage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.GetSnowballUsage"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSnowballUsage where
        toJSON = const (Object mempty)

instance ToPath GetSnowballUsage where
        toPath = const "/"

instance ToQuery GetSnowballUsage where
        toQuery = const mempty

-- | /See:/ 'getSnowballUsageResponse' smart constructor.
data GetSnowballUsageResponse = GetSnowballUsageResponse'
  { _gsursSnowballsInUse :: !(Maybe Int)
  , _gsursSnowballLimit  :: !(Maybe Int)
  , _gsursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSnowballUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsursSnowballsInUse' - The number of Snowballs that this account is currently using.
--
-- * 'gsursSnowballLimit' - The service limit for number of Snowballs this account can have at once. The default service limit is 1 (one).
--
-- * 'gsursResponseStatus' - -- | The response status code.
getSnowballUsageResponse
    :: Int -- ^ 'gsursResponseStatus'
    -> GetSnowballUsageResponse
getSnowballUsageResponse pResponseStatus_ =
  GetSnowballUsageResponse'
    { _gsursSnowballsInUse = Nothing
    , _gsursSnowballLimit = Nothing
    , _gsursResponseStatus = pResponseStatus_
    }


-- | The number of Snowballs that this account is currently using.
gsursSnowballsInUse :: Lens' GetSnowballUsageResponse (Maybe Int)
gsursSnowballsInUse = lens _gsursSnowballsInUse (\ s a -> s{_gsursSnowballsInUse = a})

-- | The service limit for number of Snowballs this account can have at once. The default service limit is 1 (one).
gsursSnowballLimit :: Lens' GetSnowballUsageResponse (Maybe Int)
gsursSnowballLimit = lens _gsursSnowballLimit (\ s a -> s{_gsursSnowballLimit = a})

-- | -- | The response status code.
gsursResponseStatus :: Lens' GetSnowballUsageResponse Int
gsursResponseStatus = lens _gsursResponseStatus (\ s a -> s{_gsursResponseStatus = a})

instance NFData GetSnowballUsageResponse where
