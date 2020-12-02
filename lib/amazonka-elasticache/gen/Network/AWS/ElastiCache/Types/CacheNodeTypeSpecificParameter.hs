{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter where

import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
import Network.AWS.ElastiCache.Types.ChangeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a @cache.m1.large@ cache node type would have a larger @maxmemory@ value than a @cache.m1.small@ type.
--
--
--
-- /See:/ 'cacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { _cntspCacheNodeTypeSpecificValues ::
      !( Maybe
           [CacheNodeTypeSpecificValue]
       ),
    _cntspMinimumEngineVersion ::
      !(Maybe Text),
    _cntspSource :: !(Maybe Text),
    _cntspIsModifiable ::
      !(Maybe Bool),
    _cntspDataType ::
      !(Maybe Text),
    _cntspAllowedValues ::
      !(Maybe Text),
    _cntspParameterName ::
      !(Maybe Text),
    _cntspDescription ::
      !(Maybe Text),
    _cntspChangeType ::
      !(Maybe ChangeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheNodeTypeSpecificParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntspCacheNodeTypeSpecificValues' - A list of cache node types and their corresponding values for this parameter.
--
-- * 'cntspMinimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
--
-- * 'cntspSource' - The source of the parameter value.
--
-- * 'cntspIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'cntspDataType' - The valid data type for the parameter.
--
-- * 'cntspAllowedValues' - The valid range of values for the parameter.
--
-- * 'cntspParameterName' - The name of the parameter.
--
-- * 'cntspDescription' - A description of the parameter.
--
-- * 'cntspChangeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
cacheNodeTypeSpecificParameter ::
  CacheNodeTypeSpecificParameter
cacheNodeTypeSpecificParameter =
  CacheNodeTypeSpecificParameter'
    { _cntspCacheNodeTypeSpecificValues =
        Nothing,
      _cntspMinimumEngineVersion = Nothing,
      _cntspSource = Nothing,
      _cntspIsModifiable = Nothing,
      _cntspDataType = Nothing,
      _cntspAllowedValues = Nothing,
      _cntspParameterName = Nothing,
      _cntspDescription = Nothing,
      _cntspChangeType = Nothing
    }

-- | A list of cache node types and their corresponding values for this parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter [CacheNodeTypeSpecificValue]
cntspCacheNodeTypeSpecificValues = lens _cntspCacheNodeTypeSpecificValues (\s a -> s {_cntspCacheNodeTypeSpecificValues = a}) . _Default . _Coerce

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion = lens _cntspMinimumEngineVersion (\s a -> s {_cntspMinimumEngineVersion = a})

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\s a -> s {_cntspSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable = lens _cntspIsModifiable (\s a -> s {_cntspIsModifiable = a})

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\s a -> s {_cntspDataType = a})

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues = lens _cntspAllowedValues (\s a -> s {_cntspAllowedValues = a})

-- | The name of the parameter.
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName = lens _cntspParameterName (\s a -> s {_cntspParameterName = a})

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription = lens _cntspDescription (\s a -> s {_cntspDescription = a})

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
cntspChangeType :: Lens' CacheNodeTypeSpecificParameter (Maybe ChangeType)
cntspChangeType = lens _cntspChangeType (\s a -> s {_cntspChangeType = a})

instance FromXML CacheNodeTypeSpecificParameter where
  parseXML x =
    CacheNodeTypeSpecificParameter'
      <$> ( x .@? "CacheNodeTypeSpecificValues" .!@ mempty
              >>= may (parseXMLList "CacheNodeTypeSpecificValue")
          )
      <*> (x .@? "MinimumEngineVersion")
      <*> (x .@? "Source")
      <*> (x .@? "IsModifiable")
      <*> (x .@? "DataType")
      <*> (x .@? "AllowedValues")
      <*> (x .@? "ParameterName")
      <*> (x .@? "Description")
      <*> (x .@? "ChangeType")

instance Hashable CacheNodeTypeSpecificParameter

instance NFData CacheNodeTypeSpecificParameter
