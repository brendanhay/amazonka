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
-- Module      : Network.AWS.CloudFront.GetKeyGroupConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key group configuration.
--
--
-- To get a key group configuration, you must provide the key group’s identifier. If the key group is referenced in a distribution’s cache behavior, you can get the key group’s identifier using @ListDistributions@ or @GetDistribution@ . If the key group is not referenced in a cache behavior, you can get the identifier using @ListKeyGroups@ .
module Network.AWS.CloudFront.GetKeyGroupConfig
  ( -- * Creating a Request
    getKeyGroupConfig,
    GetKeyGroupConfig,

    -- * Request Lenses
    gkgcId,

    -- * Destructuring the Response
    getKeyGroupConfigResponse,
    GetKeyGroupConfigResponse,

    -- * Response Lenses
    gkgcrsETag,
    gkgcrsKeyGroupConfig,
    gkgcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyGroupConfig' smart constructor.
newtype GetKeyGroupConfig = GetKeyGroupConfig' {_gkgcId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyGroupConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkgcId' - The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
getKeyGroupConfig ::
  -- | 'gkgcId'
  Text ->
  GetKeyGroupConfig
getKeyGroupConfig pId_ = GetKeyGroupConfig' {_gkgcId = pId_}

-- | The identifier of the key group whose configuration you are getting. To get the identifier, use @ListKeyGroups@ .
gkgcId :: Lens' GetKeyGroupConfig Text
gkgcId = lens _gkgcId (\s a -> s {_gkgcId = a})

instance AWSRequest GetKeyGroupConfig where
  type Rs GetKeyGroupConfig = GetKeyGroupConfigResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetKeyGroupConfigResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetKeyGroupConfig

instance NFData GetKeyGroupConfig

instance ToHeaders GetKeyGroupConfig where
  toHeaders = const mempty

instance ToPath GetKeyGroupConfig where
  toPath GetKeyGroupConfig' {..} =
    mconcat ["/2020-05-31/key-group/", toBS _gkgcId, "/config"]

instance ToQuery GetKeyGroupConfig where
  toQuery = const mempty

-- | /See:/ 'getKeyGroupConfigResponse' smart constructor.
data GetKeyGroupConfigResponse = GetKeyGroupConfigResponse'
  { _gkgcrsETag ::
      !(Maybe Text),
    _gkgcrsKeyGroupConfig ::
      !(Maybe KeyGroupConfig),
    _gkgcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyGroupConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkgcrsETag' - The identifier for this version of the key group.
--
-- * 'gkgcrsKeyGroupConfig' - The key group configuration.
--
-- * 'gkgcrsResponseStatus' - -- | The response status code.
getKeyGroupConfigResponse ::
  -- | 'gkgcrsResponseStatus'
  Int ->
  GetKeyGroupConfigResponse
getKeyGroupConfigResponse pResponseStatus_ =
  GetKeyGroupConfigResponse'
    { _gkgcrsETag = Nothing,
      _gkgcrsKeyGroupConfig = Nothing,
      _gkgcrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this version of the key group.
gkgcrsETag :: Lens' GetKeyGroupConfigResponse (Maybe Text)
gkgcrsETag = lens _gkgcrsETag (\s a -> s {_gkgcrsETag = a})

-- | The key group configuration.
gkgcrsKeyGroupConfig :: Lens' GetKeyGroupConfigResponse (Maybe KeyGroupConfig)
gkgcrsKeyGroupConfig = lens _gkgcrsKeyGroupConfig (\s a -> s {_gkgcrsKeyGroupConfig = a})

-- | -- | The response status code.
gkgcrsResponseStatus :: Lens' GetKeyGroupConfigResponse Int
gkgcrsResponseStatus = lens _gkgcrsResponseStatus (\s a -> s {_gkgcrsResponseStatus = a})

instance NFData GetKeyGroupConfigResponse
