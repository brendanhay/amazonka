{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CachingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CachingConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The caching configuration for a resolver that has caching enabled.
--
--
--
-- /See:/ 'cachingConfig' smart constructor.
data CachingConfig = CachingConfig'
  { _ccTtl :: !(Maybe Integer),
    _ccCachingKeys :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccTtl' - The TTL in seconds for a resolver that has caching enabled. Valid values are between 1 and 3600 seconds.
--
-- * 'ccCachingKeys' - The caching keys for a resolver that has caching enabled. Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
cachingConfig ::
  CachingConfig
cachingConfig =
  CachingConfig' {_ccTtl = Nothing, _ccCachingKeys = Nothing}

-- | The TTL in seconds for a resolver that has caching enabled. Valid values are between 1 and 3600 seconds.
ccTtl :: Lens' CachingConfig (Maybe Integer)
ccTtl = lens _ccTtl (\s a -> s {_ccTtl = a})

-- | The caching keys for a resolver that has caching enabled. Valid values are entries from the @> context.arguments@ , @> context.source@ , and @> context.identity@ maps.
ccCachingKeys :: Lens' CachingConfig [Text]
ccCachingKeys = lens _ccCachingKeys (\s a -> s {_ccCachingKeys = a}) . _Default . _Coerce

instance FromJSON CachingConfig where
  parseJSON =
    withObject
      "CachingConfig"
      ( \x ->
          CachingConfig'
            <$> (x .:? "ttl") <*> (x .:? "cachingKeys" .!= mempty)
      )

instance Hashable CachingConfig

instance NFData CachingConfig

instance ToJSON CachingConfig where
  toJSON CachingConfig' {..} =
    object
      ( catMaybes
          [("ttl" .=) <$> _ccTtl, ("cachingKeys" .=) <$> _ccCachingKeys]
      )
