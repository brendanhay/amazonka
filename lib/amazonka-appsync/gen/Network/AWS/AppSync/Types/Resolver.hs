{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Resolver where

import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.ResolverKind
import Network.AWS.AppSync.Types.SyncConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a resolver.
--
--
--
-- /See:/ 'resolver' smart constructor.
data Resolver = Resolver'
  { _rTypeName :: !(Maybe Text),
    _rDataSourceName :: !(Maybe Text),
    _rRequestMappingTemplate :: !(Maybe Text),
    _rKind :: !(Maybe ResolverKind),
    _rResolverARN :: !(Maybe Text),
    _rCachingConfig :: !(Maybe CachingConfig),
    _rResponseMappingTemplate :: !(Maybe Text),
    _rFieldName :: !(Maybe Text),
    _rSyncConfig :: !(Maybe SyncConfig),
    _rPipelineConfig :: !(Maybe PipelineConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rTypeName' - The resolver type name.
--
-- * 'rDataSourceName' - The resolver data source name.
--
-- * 'rRequestMappingTemplate' - The request mapping template.
--
-- * 'rKind' - The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
-- * 'rResolverARN' - The resolver ARN.
--
-- * 'rCachingConfig' - The caching configuration for the resolver.
--
-- * 'rResponseMappingTemplate' - The response mapping template.
--
-- * 'rFieldName' - The resolver field name.
--
-- * 'rSyncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- * 'rPipelineConfig' - The @PipelineConfig@ .
resolver ::
  Resolver
resolver =
  Resolver'
    { _rTypeName = Nothing,
      _rDataSourceName = Nothing,
      _rRequestMappingTemplate = Nothing,
      _rKind = Nothing,
      _rResolverARN = Nothing,
      _rCachingConfig = Nothing,
      _rResponseMappingTemplate = Nothing,
      _rFieldName = Nothing,
      _rSyncConfig = Nothing,
      _rPipelineConfig = Nothing
    }

-- | The resolver type name.
rTypeName :: Lens' Resolver (Maybe Text)
rTypeName = lens _rTypeName (\s a -> s {_rTypeName = a})

-- | The resolver data source name.
rDataSourceName :: Lens' Resolver (Maybe Text)
rDataSourceName = lens _rDataSourceName (\s a -> s {_rDataSourceName = a})

-- | The request mapping template.
rRequestMappingTemplate :: Lens' Resolver (Maybe Text)
rRequestMappingTemplate = lens _rRequestMappingTemplate (\s a -> s {_rRequestMappingTemplate = a})

-- | The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
rKind :: Lens' Resolver (Maybe ResolverKind)
rKind = lens _rKind (\s a -> s {_rKind = a})

-- | The resolver ARN.
rResolverARN :: Lens' Resolver (Maybe Text)
rResolverARN = lens _rResolverARN (\s a -> s {_rResolverARN = a})

-- | The caching configuration for the resolver.
rCachingConfig :: Lens' Resolver (Maybe CachingConfig)
rCachingConfig = lens _rCachingConfig (\s a -> s {_rCachingConfig = a})

-- | The response mapping template.
rResponseMappingTemplate :: Lens' Resolver (Maybe Text)
rResponseMappingTemplate = lens _rResponseMappingTemplate (\s a -> s {_rResponseMappingTemplate = a})

-- | The resolver field name.
rFieldName :: Lens' Resolver (Maybe Text)
rFieldName = lens _rFieldName (\s a -> s {_rFieldName = a})

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
rSyncConfig :: Lens' Resolver (Maybe SyncConfig)
rSyncConfig = lens _rSyncConfig (\s a -> s {_rSyncConfig = a})

-- | The @PipelineConfig@ .
rPipelineConfig :: Lens' Resolver (Maybe PipelineConfig)
rPipelineConfig = lens _rPipelineConfig (\s a -> s {_rPipelineConfig = a})

instance FromJSON Resolver where
  parseJSON =
    withObject
      "Resolver"
      ( \x ->
          Resolver'
            <$> (x .:? "typeName")
            <*> (x .:? "dataSourceName")
            <*> (x .:? "requestMappingTemplate")
            <*> (x .:? "kind")
            <*> (x .:? "resolverArn")
            <*> (x .:? "cachingConfig")
            <*> (x .:? "responseMappingTemplate")
            <*> (x .:? "fieldName")
            <*> (x .:? "syncConfig")
            <*> (x .:? "pipelineConfig")
      )

instance Hashable Resolver

instance NFData Resolver
