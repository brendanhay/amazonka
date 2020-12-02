{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A value that applies only to a certain cache node type.
--
--
--
-- /See:/ 'cacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
  { _cntsvCacheNodeType ::
      !(Maybe Text),
    _cntsvValue :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheNodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntsvCacheNodeType' - The cache node type for which this value applies.
--
-- * 'cntsvValue' - The value for the cache node type.
cacheNodeTypeSpecificValue ::
  CacheNodeTypeSpecificValue
cacheNodeTypeSpecificValue =
  CacheNodeTypeSpecificValue'
    { _cntsvCacheNodeType = Nothing,
      _cntsvValue = Nothing
    }

-- | The cache node type for which this value applies.
cntsvCacheNodeType :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvCacheNodeType = lens _cntsvCacheNodeType (\s a -> s {_cntsvCacheNodeType = a})

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue = lens _cntsvValue (\s a -> s {_cntsvValue = a})

instance FromXML CacheNodeTypeSpecificValue where
  parseXML x =
    CacheNodeTypeSpecificValue'
      <$> (x .@? "CacheNodeType") <*> (x .@? "Value")

instance Hashable CacheNodeTypeSpecificValue

instance NFData CacheNodeTypeSpecificValue
