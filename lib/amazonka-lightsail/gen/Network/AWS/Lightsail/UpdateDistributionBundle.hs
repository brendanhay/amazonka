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
-- Module      : Network.AWS.Lightsail.UpdateDistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle of your Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- A distribution bundle specifies the monthly network transfer quota and monthly cost of your dsitribution.
--
-- Update your distribution's bundle if your distribution is going over its monthly network transfer quota and is incurring an overage fee.
--
-- You can update your distribution's bundle only one time within your monthly AWS billing cycle. To determine if you can update your distribution's bundle, use the @GetDistributions@ action. The @ableToUpdateBundle@ parameter in the result will indicate whether you can currently update your distribution's bundle.
module Network.AWS.Lightsail.UpdateDistributionBundle
  ( -- * Creating a Request
    updateDistributionBundle,
    UpdateDistributionBundle,

    -- * Request Lenses
    udbBundleId,
    udbDistributionName,

    -- * Destructuring the Response
    updateDistributionBundleResponse,
    UpdateDistributionBundleResponse,

    -- * Response Lenses
    udbrsOperation,
    udbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDistributionBundle' smart constructor.
data UpdateDistributionBundle = UpdateDistributionBundle'
  { _udbBundleId ::
      !(Maybe Text),
    _udbDistributionName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDistributionBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udbBundleId' - The bundle ID of the new bundle to apply to your distribution. Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
--
-- * 'udbDistributionName' - The name of the distribution for which to update the bundle. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
updateDistributionBundle ::
  UpdateDistributionBundle
updateDistributionBundle =
  UpdateDistributionBundle'
    { _udbBundleId = Nothing,
      _udbDistributionName = Nothing
    }

-- | The bundle ID of the new bundle to apply to your distribution. Use the @GetDistributionBundles@ action to get a list of distribution bundle IDs that you can specify.
udbBundleId :: Lens' UpdateDistributionBundle (Maybe Text)
udbBundleId = lens _udbBundleId (\s a -> s {_udbBundleId = a})

-- | The name of the distribution for which to update the bundle. Use the @GetDistributions@ action to get a list of distribution names that you can specify.
udbDistributionName :: Lens' UpdateDistributionBundle (Maybe Text)
udbDistributionName = lens _udbDistributionName (\s a -> s {_udbDistributionName = a})

instance AWSRequest UpdateDistributionBundle where
  type Rs UpdateDistributionBundle = UpdateDistributionBundleResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          UpdateDistributionBundleResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable UpdateDistributionBundle

instance NFData UpdateDistributionBundle

instance ToHeaders UpdateDistributionBundle where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.UpdateDistributionBundle" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateDistributionBundle where
  toJSON UpdateDistributionBundle' {..} =
    object
      ( catMaybes
          [ ("bundleId" .=) <$> _udbBundleId,
            ("distributionName" .=) <$> _udbDistributionName
          ]
      )

instance ToPath UpdateDistributionBundle where
  toPath = const "/"

instance ToQuery UpdateDistributionBundle where
  toQuery = const mempty

-- | /See:/ 'updateDistributionBundleResponse' smart constructor.
data UpdateDistributionBundleResponse = UpdateDistributionBundleResponse'
  { _udbrsOperation ::
      !(Maybe Operation),
    _udbrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDistributionBundleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udbrsOperation' - Undocumented member.
--
-- * 'udbrsResponseStatus' - -- | The response status code.
updateDistributionBundleResponse ::
  -- | 'udbrsResponseStatus'
  Int ->
  UpdateDistributionBundleResponse
updateDistributionBundleResponse pResponseStatus_ =
  UpdateDistributionBundleResponse'
    { _udbrsOperation = Nothing,
      _udbrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
udbrsOperation :: Lens' UpdateDistributionBundleResponse (Maybe Operation)
udbrsOperation = lens _udbrsOperation (\s a -> s {_udbrsOperation = a})

-- | -- | The response status code.
udbrsResponseStatus :: Lens' UpdateDistributionBundleResponse Int
udbrsResponseStatus = lens _udbrsResponseStatus (\s a -> s {_udbrsResponseStatus = a})

instance NFData UpdateDistributionBundleResponse
