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
-- Module      : Network.AWS.AppSync.UpdateResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a @Resolver@ object.
module Network.AWS.AppSync.UpdateResolver
  ( -- * Creating a Request
    updateResolver,
    UpdateResolver,

    -- * Request Lenses
    urDataSourceName,
    urRequestMappingTemplate,
    urKind,
    urCachingConfig,
    urResponseMappingTemplate,
    urSyncConfig,
    urPipelineConfig,
    urApiId,
    urTypeName,
    urFieldName,

    -- * Destructuring the Response
    updateResolverResponse,
    UpdateResolverResponse,

    -- * Response Lenses
    urrsResolver,
    urrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateResolver' smart constructor.
data UpdateResolver = UpdateResolver'
  { _urDataSourceName ::
      !(Maybe Text),
    _urRequestMappingTemplate :: !(Maybe Text),
    _urKind :: !(Maybe ResolverKind),
    _urCachingConfig :: !(Maybe CachingConfig),
    _urResponseMappingTemplate :: !(Maybe Text),
    _urSyncConfig :: !(Maybe SyncConfig),
    _urPipelineConfig :: !(Maybe PipelineConfig),
    _urApiId :: !Text,
    _urTypeName :: !Text,
    _urFieldName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateResolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urDataSourceName' - The new data source name.
--
-- * 'urRequestMappingTemplate' - The new request mapping template. A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL). VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
--
-- * 'urKind' - The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
-- * 'urCachingConfig' - The caching configuration for the resolver.
--
-- * 'urResponseMappingTemplate' - The new response mapping template.
--
-- * 'urSyncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- * 'urPipelineConfig' - The @PipelineConfig@ .
--
-- * 'urApiId' - The API ID.
--
-- * 'urTypeName' - The new type name.
--
-- * 'urFieldName' - The new field name.
updateResolver ::
  -- | 'urApiId'
  Text ->
  -- | 'urTypeName'
  Text ->
  -- | 'urFieldName'
  Text ->
  UpdateResolver
updateResolver pApiId_ pTypeName_ pFieldName_ =
  UpdateResolver'
    { _urDataSourceName = Nothing,
      _urRequestMappingTemplate = Nothing,
      _urKind = Nothing,
      _urCachingConfig = Nothing,
      _urResponseMappingTemplate = Nothing,
      _urSyncConfig = Nothing,
      _urPipelineConfig = Nothing,
      _urApiId = pApiId_,
      _urTypeName = pTypeName_,
      _urFieldName = pFieldName_
    }

-- | The new data source name.
urDataSourceName :: Lens' UpdateResolver (Maybe Text)
urDataSourceName = lens _urDataSourceName (\s a -> s {_urDataSourceName = a})

-- | The new request mapping template. A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL). VTL request mapping templates are optional when using a Lambda data source. For all other data sources, VTL request and response mapping templates are required.
urRequestMappingTemplate :: Lens' UpdateResolver (Maybe Text)
urRequestMappingTemplate = lens _urRequestMappingTemplate (\s a -> s {_urRequestMappingTemplate = a})

-- | The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
urKind :: Lens' UpdateResolver (Maybe ResolverKind)
urKind = lens _urKind (\s a -> s {_urKind = a})

-- | The caching configuration for the resolver.
urCachingConfig :: Lens' UpdateResolver (Maybe CachingConfig)
urCachingConfig = lens _urCachingConfig (\s a -> s {_urCachingConfig = a})

-- | The new response mapping template.
urResponseMappingTemplate :: Lens' UpdateResolver (Maybe Text)
urResponseMappingTemplate = lens _urResponseMappingTemplate (\s a -> s {_urResponseMappingTemplate = a})

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
urSyncConfig :: Lens' UpdateResolver (Maybe SyncConfig)
urSyncConfig = lens _urSyncConfig (\s a -> s {_urSyncConfig = a})

-- | The @PipelineConfig@ .
urPipelineConfig :: Lens' UpdateResolver (Maybe PipelineConfig)
urPipelineConfig = lens _urPipelineConfig (\s a -> s {_urPipelineConfig = a})

-- | The API ID.
urApiId :: Lens' UpdateResolver Text
urApiId = lens _urApiId (\s a -> s {_urApiId = a})

-- | The new type name.
urTypeName :: Lens' UpdateResolver Text
urTypeName = lens _urTypeName (\s a -> s {_urTypeName = a})

-- | The new field name.
urFieldName :: Lens' UpdateResolver Text
urFieldName = lens _urFieldName (\s a -> s {_urFieldName = a})

instance AWSRequest UpdateResolver where
  type Rs UpdateResolver = UpdateResolverResponse
  request = postJSON appSync
  response =
    receiveJSON
      ( \s h x ->
          UpdateResolverResponse'
            <$> (x .?> "resolver") <*> (pure (fromEnum s))
      )

instance Hashable UpdateResolver

instance NFData UpdateResolver

instance ToHeaders UpdateResolver where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateResolver where
  toJSON UpdateResolver' {..} =
    object
      ( catMaybes
          [ ("dataSourceName" .=) <$> _urDataSourceName,
            ("requestMappingTemplate" .=) <$> _urRequestMappingTemplate,
            ("kind" .=) <$> _urKind,
            ("cachingConfig" .=) <$> _urCachingConfig,
            ("responseMappingTemplate" .=) <$> _urResponseMappingTemplate,
            ("syncConfig" .=) <$> _urSyncConfig,
            ("pipelineConfig" .=) <$> _urPipelineConfig
          ]
      )

instance ToPath UpdateResolver where
  toPath UpdateResolver' {..} =
    mconcat
      [ "/v1/apis/",
        toBS _urApiId,
        "/types/",
        toBS _urTypeName,
        "/resolvers/",
        toBS _urFieldName
      ]

instance ToQuery UpdateResolver where
  toQuery = const mempty

-- | /See:/ 'updateResolverResponse' smart constructor.
data UpdateResolverResponse = UpdateResolverResponse'
  { _urrsResolver ::
      !(Maybe Resolver),
    _urrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateResolverResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResolver' - The updated @Resolver@ object.
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateResolverResponse ::
  -- | 'urrsResponseStatus'
  Int ->
  UpdateResolverResponse
updateResolverResponse pResponseStatus_ =
  UpdateResolverResponse'
    { _urrsResolver = Nothing,
      _urrsResponseStatus = pResponseStatus_
    }

-- | The updated @Resolver@ object.
urrsResolver :: Lens' UpdateResolverResponse (Maybe Resolver)
urrsResolver = lens _urrsResolver (\s a -> s {_urrsResolver = a})

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateResolverResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\s a -> s {_urrsResponseStatus = a})

instance NFData UpdateResolverResponse
