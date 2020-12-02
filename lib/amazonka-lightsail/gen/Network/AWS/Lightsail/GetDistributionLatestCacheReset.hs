{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDistributionLatestCacheReset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the timestamp and status of the last cache reset of a specific Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.GetDistributionLatestCacheReset
  ( -- * Creating a Request
    getDistributionLatestCacheReset,
    GetDistributionLatestCacheReset,

    -- * Request Lenses
    gdlcrDistributionName,

    -- * Destructuring the Response
    getDistributionLatestCacheResetResponse,
    GetDistributionLatestCacheResetResponse,

    -- * Response Lenses
    gdlcrrsStatus,
    gdlcrrsCreateTime,
    gdlcrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDistributionLatestCacheReset' smart constructor.
newtype GetDistributionLatestCacheReset = GetDistributionLatestCacheReset'
  { _gdlcrDistributionName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributionLatestCacheReset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdlcrDistributionName' - The name of the distribution for which to return the timestamp of the last cache reset. Use the @GetDistributions@ action to get a list of distribution names that you can specify. When omitted, the response includes the latest cache reset timestamp of all your distributions.
getDistributionLatestCacheReset ::
  GetDistributionLatestCacheReset
getDistributionLatestCacheReset =
  GetDistributionLatestCacheReset'
    { _gdlcrDistributionName =
        Nothing
    }

-- | The name of the distribution for which to return the timestamp of the last cache reset. Use the @GetDistributions@ action to get a list of distribution names that you can specify. When omitted, the response includes the latest cache reset timestamp of all your distributions.
gdlcrDistributionName :: Lens' GetDistributionLatestCacheReset (Maybe Text)
gdlcrDistributionName = lens _gdlcrDistributionName (\s a -> s {_gdlcrDistributionName = a})

instance AWSRequest GetDistributionLatestCacheReset where
  type
    Rs GetDistributionLatestCacheReset =
      GetDistributionLatestCacheResetResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetDistributionLatestCacheResetResponse'
            <$> (x .?> "status") <*> (x .?> "createTime") <*> (pure (fromEnum s))
      )

instance Hashable GetDistributionLatestCacheReset

instance NFData GetDistributionLatestCacheReset

instance ToHeaders GetDistributionLatestCacheReset where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.GetDistributionLatestCacheReset" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDistributionLatestCacheReset where
  toJSON GetDistributionLatestCacheReset' {..} =
    object
      (catMaybes [("distributionName" .=) <$> _gdlcrDistributionName])

instance ToPath GetDistributionLatestCacheReset where
  toPath = const "/"

instance ToQuery GetDistributionLatestCacheReset where
  toQuery = const mempty

-- | /See:/ 'getDistributionLatestCacheResetResponse' smart constructor.
data GetDistributionLatestCacheResetResponse = GetDistributionLatestCacheResetResponse'
  { _gdlcrrsStatus ::
      !( Maybe
           Text
       ),
    _gdlcrrsCreateTime ::
      !( Maybe
           POSIX
       ),
    _gdlcrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributionLatestCacheResetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdlcrrsStatus' - The status of the last cache reset.
--
-- * 'gdlcrrsCreateTime' - The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
--
-- * 'gdlcrrsResponseStatus' - -- | The response status code.
getDistributionLatestCacheResetResponse ::
  -- | 'gdlcrrsResponseStatus'
  Int ->
  GetDistributionLatestCacheResetResponse
getDistributionLatestCacheResetResponse pResponseStatus_ =
  GetDistributionLatestCacheResetResponse'
    { _gdlcrrsStatus =
        Nothing,
      _gdlcrrsCreateTime = Nothing,
      _gdlcrrsResponseStatus = pResponseStatus_
    }

-- | The status of the last cache reset.
gdlcrrsStatus :: Lens' GetDistributionLatestCacheResetResponse (Maybe Text)
gdlcrrsStatus = lens _gdlcrrsStatus (\s a -> s {_gdlcrrsStatus = a})

-- | The timestamp of the last cache reset (e.g., @1479734909.17@ ) in Unix time format.
gdlcrrsCreateTime :: Lens' GetDistributionLatestCacheResetResponse (Maybe UTCTime)
gdlcrrsCreateTime = lens _gdlcrrsCreateTime (\s a -> s {_gdlcrrsCreateTime = a}) . mapping _Time

-- | -- | The response status code.
gdlcrrsResponseStatus :: Lens' GetDistributionLatestCacheResetResponse Int
gdlcrrsResponseStatus = lens _gdlcrrsResponseStatus (\s a -> s {_gdlcrrsResponseStatus = a})

instance NFData GetDistributionLatestCacheResetResponse
