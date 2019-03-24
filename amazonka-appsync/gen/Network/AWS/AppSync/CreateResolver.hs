{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateResolver
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Resolver@ object.
--
--
-- A resolver converts incoming requests into a format that a data source can understand and converts the data source's responses into GraphQL.
--
module Network.AWS.AppSync.CreateResolver
    (
    -- * Creating a Request
      createResolver
    , CreateResolver
    -- * Request Lenses
    , crDataSourceName
    , crKind
    , crResponseMappingTemplate
    , crPipelineConfig
    , crApiId
    , crTypeName
    , crFieldName
    , crRequestMappingTemplate

    -- * Destructuring the Response
    , createResolverResponse
    , CreateResolverResponse
    -- * Response Lenses
    , crrsResolver
    , crrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createResolver' smart constructor.
data CreateResolver = CreateResolver'
  { _crDataSourceName          :: !(Maybe Text)
  , _crKind                    :: !(Maybe ResolverKind)
  , _crResponseMappingTemplate :: !(Maybe Text)
  , _crPipelineConfig          :: !(Maybe PipelineConfig)
  , _crApiId                   :: !Text
  , _crTypeName                :: !Text
  , _crFieldName               :: !Text
  , _crRequestMappingTemplate  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResolver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crDataSourceName' - The name of the data source for which the resolver is being created.
--
-- * 'crKind' - The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
-- * 'crResponseMappingTemplate' - The mapping template to be used for responses from the data source.
--
-- * 'crPipelineConfig' - The @PipelineConfig@ .
--
-- * 'crApiId' - The ID for the GraphQL API for which the resolver is being created.
--
-- * 'crTypeName' - The name of the @Type@ .
--
-- * 'crFieldName' - The name of the field to attach the resolver to.
--
-- * 'crRequestMappingTemplate' - The mapping template to be used for requests. A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
createResolver
    :: Text -- ^ 'crApiId'
    -> Text -- ^ 'crTypeName'
    -> Text -- ^ 'crFieldName'
    -> Text -- ^ 'crRequestMappingTemplate'
    -> CreateResolver
createResolver pApiId_ pTypeName_ pFieldName_ pRequestMappingTemplate_ =
  CreateResolver'
    { _crDataSourceName = Nothing
    , _crKind = Nothing
    , _crResponseMappingTemplate = Nothing
    , _crPipelineConfig = Nothing
    , _crApiId = pApiId_
    , _crTypeName = pTypeName_
    , _crFieldName = pFieldName_
    , _crRequestMappingTemplate = pRequestMappingTemplate_
    }


-- | The name of the data source for which the resolver is being created.
crDataSourceName :: Lens' CreateResolver (Maybe Text)
crDataSourceName = lens _crDataSourceName (\ s a -> s{_crDataSourceName = a})

-- | The resolver type.     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
crKind :: Lens' CreateResolver (Maybe ResolverKind)
crKind = lens _crKind (\ s a -> s{_crKind = a})

-- | The mapping template to be used for responses from the data source.
crResponseMappingTemplate :: Lens' CreateResolver (Maybe Text)
crResponseMappingTemplate = lens _crResponseMappingTemplate (\ s a -> s{_crResponseMappingTemplate = a})

-- | The @PipelineConfig@ .
crPipelineConfig :: Lens' CreateResolver (Maybe PipelineConfig)
crPipelineConfig = lens _crPipelineConfig (\ s a -> s{_crPipelineConfig = a})

-- | The ID for the GraphQL API for which the resolver is being created.
crApiId :: Lens' CreateResolver Text
crApiId = lens _crApiId (\ s a -> s{_crApiId = a})

-- | The name of the @Type@ .
crTypeName :: Lens' CreateResolver Text
crTypeName = lens _crTypeName (\ s a -> s{_crTypeName = a})

-- | The name of the field to attach the resolver to.
crFieldName :: Lens' CreateResolver Text
crFieldName = lens _crFieldName (\ s a -> s{_crFieldName = a})

-- | The mapping template to be used for requests. A resolver uses a request mapping template to convert a GraphQL expression into a format that a data source can understand. Mapping templates are written in Apache Velocity Template Language (VTL).
crRequestMappingTemplate :: Lens' CreateResolver Text
crRequestMappingTemplate = lens _crRequestMappingTemplate (\ s a -> s{_crRequestMappingTemplate = a})

instance AWSRequest CreateResolver where
        type Rs CreateResolver = CreateResolverResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 CreateResolverResponse' <$>
                   (x .?> "resolver") <*> (pure (fromEnum s)))

instance Hashable CreateResolver where

instance NFData CreateResolver where

instance ToHeaders CreateResolver where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateResolver where
        toJSON CreateResolver'{..}
          = object
              (catMaybes
                 [("dataSourceName" .=) <$> _crDataSourceName,
                  ("kind" .=) <$> _crKind,
                  ("responseMappingTemplate" .=) <$>
                    _crResponseMappingTemplate,
                  ("pipelineConfig" .=) <$> _crPipelineConfig,
                  Just ("fieldName" .= _crFieldName),
                  Just
                    ("requestMappingTemplate" .=
                       _crRequestMappingTemplate)])

instance ToPath CreateResolver where
        toPath CreateResolver'{..}
          = mconcat
              ["/v1/apis/", toBS _crApiId, "/types/",
               toBS _crTypeName, "/resolvers"]

instance ToQuery CreateResolver where
        toQuery = const mempty

-- | /See:/ 'createResolverResponse' smart constructor.
data CreateResolverResponse = CreateResolverResponse'
  { _crrsResolver       :: !(Maybe Resolver)
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResolverResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsResolver' - The @Resolver@ object.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createResolverResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateResolverResponse
createResolverResponse pResponseStatus_ =
  CreateResolverResponse'
    {_crrsResolver = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | The @Resolver@ object.
crrsResolver :: Lens' CreateResolverResponse (Maybe Resolver)
crrsResolver = lens _crrsResolver (\ s a -> s{_crrsResolver = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateResolverResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateResolverResponse where
