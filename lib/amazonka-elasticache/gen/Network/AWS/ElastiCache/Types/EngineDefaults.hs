{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.EngineDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.EngineDefaults where

import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Network.AWS.ElastiCache.Types.Parameter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @DescribeEngineDefaultParameters@ operation.
--
--
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { _edCacheParameterGroupFamily ::
      !(Maybe Text),
    _edCacheNodeTypeSpecificParameters ::
      !(Maybe [CacheNodeTypeSpecificParameter]),
    _edMarker :: !(Maybe Text),
    _edParameters :: !(Maybe [Parameter])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edCacheParameterGroupFamily' - Specifies the name of the cache parameter group family to which the engine default parameters apply. Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
--
-- * 'edCacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- * 'edMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'edParameters' - Contains a list of engine default parameters.
engineDefaults ::
  EngineDefaults
engineDefaults =
  EngineDefaults'
    { _edCacheParameterGroupFamily = Nothing,
      _edCacheNodeTypeSpecificParameters = Nothing,
      _edMarker = Nothing,
      _edParameters = Nothing
    }

-- | Specifies the name of the cache parameter group family to which the engine default parameters apply. Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ | @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ | @redis6.x@ |
edCacheParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edCacheParameterGroupFamily = lens _edCacheParameterGroupFamily (\s a -> s {_edCacheParameterGroupFamily = a})

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults [CacheNodeTypeSpecificParameter]
edCacheNodeTypeSpecificParameters = lens _edCacheNodeTypeSpecificParameters (\s a -> s {_edCacheNodeTypeSpecificParameters = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s {_edMarker = a})

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\s a -> s {_edParameters = a}) . _Default . _Coerce

instance FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      <$> (x .@? "CacheParameterGroupFamily")
      <*> ( x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
              >>= may (parseXMLList "CacheNodeTypeSpecificParameter")
          )
      <*> (x .@? "Marker")
      <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "Parameter"))

instance Hashable EngineDefaults

instance NFData EngineDefaults
