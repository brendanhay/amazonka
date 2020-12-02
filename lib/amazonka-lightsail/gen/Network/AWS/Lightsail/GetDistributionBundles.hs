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
-- Module      : Network.AWS.Lightsail.GetDistributionBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list bundles that can be applied to you Amazon Lightsail content delivery network (CDN) distributions.
--
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
module Network.AWS.Lightsail.GetDistributionBundles
  ( -- * Creating a Request
    getDistributionBundles,
    GetDistributionBundles,

    -- * Destructuring the Response
    getDistributionBundlesResponse,
    GetDistributionBundlesResponse,

    -- * Response Lenses
    gdbrsBundles,
    gdbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDistributionBundles' smart constructor.
data GetDistributionBundles = GetDistributionBundles'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributionBundles' with the minimum fields required to make a request.
getDistributionBundles ::
  GetDistributionBundles
getDistributionBundles = GetDistributionBundles'

instance AWSRequest GetDistributionBundles where
  type Rs GetDistributionBundles = GetDistributionBundlesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetDistributionBundlesResponse'
            <$> (x .?> "bundles" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetDistributionBundles

instance NFData GetDistributionBundles

instance ToHeaders GetDistributionBundles where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetDistributionBundles" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDistributionBundles where
  toJSON = const (Object mempty)

instance ToPath GetDistributionBundles where
  toPath = const "/"

instance ToQuery GetDistributionBundles where
  toQuery = const mempty

-- | /See:/ 'getDistributionBundlesResponse' smart constructor.
data GetDistributionBundlesResponse = GetDistributionBundlesResponse'
  { _gdbrsBundles ::
      !(Maybe [DistributionBundle]),
    _gdbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDistributionBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdbrsBundles' - An object that describes a distribution bundle.
--
-- * 'gdbrsResponseStatus' - -- | The response status code.
getDistributionBundlesResponse ::
  -- | 'gdbrsResponseStatus'
  Int ->
  GetDistributionBundlesResponse
getDistributionBundlesResponse pResponseStatus_ =
  GetDistributionBundlesResponse'
    { _gdbrsBundles = Nothing,
      _gdbrsResponseStatus = pResponseStatus_
    }

-- | An object that describes a distribution bundle.
gdbrsBundles :: Lens' GetDistributionBundlesResponse [DistributionBundle]
gdbrsBundles = lens _gdbrsBundles (\s a -> s {_gdbrsBundles = a}) . _Default . _Coerce

-- | -- | The response status code.
gdbrsResponseStatus :: Lens' GetDistributionBundlesResponse Int
gdbrsResponseStatus = lens _gdbrsResponseStatus (\s a -> s {_gdbrsResponseStatus = a})

instance NFData GetDistributionBundlesResponse
